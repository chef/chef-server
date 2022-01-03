#
# Copyright:: Chef Software, Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

require_relative './preflight_checks'
require_relative './opensearch'

class OpensearchPreflightValidator < PreflightValidator
  # This check used to verify that the external PG version matches the version
  # we ship. When we bumped the version we ship to 9.6, we haven't yet
  # introduced any changes that _require_ 9.6. So, these constants reflect the
  # actually required PG version.
  REQUIRED_VERSION  = 1

  # supported PG version
  SUPPORTED_VERSION = 1

  # The cs_*attr variables hold the user-defined configuration
  attr_reader :cfg_opensearch_attr, :cfg_erchef_attr, :helper

  # The node_*attr variables hold the default configuration
  attr_reader :default_opensearch_attr, :default_erchef_attr

  def initialize(node)
    super
    @cfg_opensearch_attr = PrivateChef['opensearch']
    @helper = OmnibusHelper.new(node)
    @cfg_erchef_attr = PrivateChef['opscode_erchef']

    @default_opensearch_attr = node['private_chef']['opensearch']
    @default_erchef_attr = node['private_chef']['opscode-erchef']
  end

  def run!
    unless external?
      verify_system_memory
    end
    if external?
      verify_opensearch_credentials_if_external
    end
    # verify_heap_size
    verify_consistent_reindex_sleep_times
    # Should be enabled after adding internl OpenSearch support
    # verify_op_disabled_if_user_set_external_opensearch
    verify_external_url
    verify_erchef_config
    verify_opensearch_version
  end

  def verify_opensearch_version
    # Make sure the server is a supported version.
    v = search_engine_major_version(5)
    if v == REQUIRED_VERSION || v == SUPPORTED_VERSION
      :ok
    elsif v < SUPPORTED_VERSION
      ChefServer::Warnings.warn err_OPENSEARCH009_unsupported_opensearch_version(v)
    end
  end

  def search_engine_url
    if external?
      cfg_opensearch_attr['external_url']
    else
      "http://#{helper.normalize_host(default_opensearch_attr['vip'])}:#{default_opensearch_attr['port']}"
    end
  end

  def search_engine_auth_header
    auth =
    if !cfg_erchef_attr['search_auth_username'].nil? && !cfg_erchef_attr['search_auth_password'].nil?
      Base64.strict_encode64("#{cfg_erchef_attr['search_auth_username']}:#{cfg_erchef_attr['search_auth_password']}")
    else
      Base64.strict_encode64("#{default_erchef_attr['search_auth_username']}:#{default_erchef_attr['search_auth_password']}")
    end
    { Authorization: "Basic #{auth}" }
  end

  def search_engine_major_version(count)
    begin
      client = Chef::HTTP.new(search_engine_url)
      response = client.get('', search_engine_auth_header)
      JSON.parse(response)['version']['number'].split('.').first.to_i
    rescue StandardError => e
      if count.positive?
        Chef::Log.debug('Could not connect to OpenSearch, retrying in 5 seconds')
        sleep 5
        search_engine_major_version(count - 1)
      else
        Chef::Log.fatal("Could not connect to OpenSearch. Response: #{e}")
        0
      end
    end
  end

  def verify_consistent_reindex_sleep_times
    final_min = cfg_erchef_attr['reindex_sleep_min_ms'] || default_erchef_attr['reindex_sleep_min_ms']
    final_max = cfg_erchef_attr['reindex_sleep_max_ms'] || default_erchef_attr['reindex_sleep_max_ms']
    if final_min.to_i > final_max.to_i
      fail_with err_OPENSEARCH001_failed_validation(final_min, final_max)
    end
  end

  # checks that system has atleast 4GB memory
  def verify_system_memory
    system_memory_mb = Opensearch.node_memory_in_units(node, :total, :mb)
    # Ideally our max would be 4192 but in a lot of virtualized
    # environments the OS ends up reporting much less than 4GB of RAM
    # to userspace even if the VM is started with 4GB.
    required_memory_mb = 3072
    if system_memory_mb < required_memory_mb
      fail_with err_OPENSEARCH002_insufficient_system_memory(system_memory_mb, required_memory_mb)
    end
  end

  # checks that system specifys a heap size between 1GB and 26GB
  def verify_heap_size
    op_heap_size = cfg_opensearch_attr['heap_size'] || default_opensearch_attr['heap_size']

    min_heap = 1024
    # https://www.elastic.co/guide/en/elasticsearch/reference/current/heap-size.html
    max_heap = 26 * 1024

    if op_heap_size < min_heap || op_heap_size > max_heap
      fail_with err_OPENSEARCH003_invalid_opensearch_heap_size(op_heap_size, using_solr)
    end
  end

  # If an external search index was explicitly enabled by the user,
  # then we expect that both internal search services should be
  # disabled.
  def verify_op_disabled_if_user_set_external_opensearch
    if external? && opensearch_enabled?
      fail_with err_OPENSEARCH005_should_disable_opensearch
    elsif cfg_erchef_attr['search_auth_username'].nil? && cfg_erchef_attr['search_auth_password'].nil?
      if default_erchef_attr['search_auth_username'].nil? && default_erchef_attr['search_auth_password'].nil?
        fail_with err_OPENSEARCH006_should_auth
      end
    end
  end

  def verify_opensearch_credentials_if_external
    if cfg_erchef_attr['search_auth_username'].nil? && cfg_erchef_attr['search_auth_password'].nil?
      fail_with err_OPENSEARCH006_should_auth
    end
  end

  def external?
    cfg_opensearch_attr['external']
  end

  def opensearch_enabled?
    return false if PrivateChef['use_chef_backend']

    if cfg_opensearch_attr['enable'].nil?
      default_opensearch_attr['enable']
    else
      cfg_opensearch_attr['enable']
    end
  end

  def verify_external_url
    if cfg_opensearch_attr['external'] && !cfg_opensearch_attr['external_url']
      fail_with err_OPENSEARCH007_bad_external_config()
    end
  end

  def verify_erchef_config
    provider = @cfg_erchef_attr['search_provider']
    return true if provider.nil? # default provider

    unless %w(batch inline).include?(@cfg_erchef_attr['search_queue_mode'])
      fail_with err_OPENSEARCH008_bad_queue_mode
    end
  end

  def err_OPENSEARCH001_failed_validation(final_min, final_max)
    <<~EOM

      OPENSEARCH001: opscode_erchef['reindex_sleep_min_ms'] (#{final_min}) is greater than
                opscode_erchef['reindex_sleep_max_ms'] (#{final_max})

                The maximum sleep time should be greater or equal to the minimum sleep
                time.
    EOM
  end

  def err_OPENSEARCH002_insufficient_system_memory(system_memory, required_memory)
    <<~EOM

      OPENSEARCH002: Insufficient system memory

                System has #{system_memory} MB of memory,
                but #{required_memory} MB is required.

    EOM
  end

  def err_OPENSEARCH003_invalid_opensearch_heap_size(heap_size)
    <<~EOM
      OPENSEARCH004: Invalid OpenSearch heap size

                    opensearch['heap_size'] is #{heap_size}MB

                  The recommended heap_size is between 1GB and 26GB. Refer to
                  https://opensearch.org/docs/latest/opensearch/index/
                  for more information.
    EOM
  end

  def err_OPENSEARCH005_should_disable_opensearch
    <<~EOM

      OPENSEARCH005: The #{CHEF_SERVER_NAME} is configured to use an external search
                provider but the internal OpenSearch is still enabled. This is
                an unsupported configuration.

                To disable the internal OpenSearch, add the following to #{CHEF_SERVER_CONFIG_FILE}:

                    opensearch['enable'] = false

     EOM
  end

  def err_OPENSEARCH006_should_auth
    <<~EOM

      OPENSEARCH006: Missing username and password for OpenSearch authentication

                     The #{CHEF_SERVER_NAME} is configured to use 'opensearch` as
                     its search provider. OpenSearch requires a username and password.

                     To correct this, please add the following to #{CHEF_SERVER_CONFIG_FILE}:

                      opscode_erchef['search_auth_username'] = OPENSEARCH_USERNAME
                      opscode_erchef['search_auth_password'] = OPENSEARCH_PASSWORD
     EOM
  end

  def err_OPENSEARCH007_bad_external_config
    <<~EOM

      OPENSEARCH007: external url required for OpenSearch.
                     When using external OpenSearch, a URL for the OpenSearch server must be provided.

                     To correct this, please add the following to #{CHEF_SERVER_CONFIG_FILE}:

                      opensearch['external'] = true
                      opensearch['external_url'] = YOUR_OPENSEARCH_URL
    EOM
  end

  def err_OPENSEARCH008_bad_queue_mode
    <<~EOM

      OPENSEARCH008: Invalid search queue mode.
                     The 'opensearch' provider requires either 'batch' or 'inline' value for
                     `search_queue_mode`.

                     To correct this, please add one of the following to #{CHEF_SERVER_CONFIG_FILE}:
                        opscode_erchef['search_queue_mode'] = 'inline'
                     or
                        opscode_erchef['search_queue_mode'] = 'batch'

                     information about which of these modes is correct for your configuration can be
                     found here: https://docs.chef.io/server/config_rb_server_optional_settings
    EOM
  end

  def err_OPENSEARCH009_unsupported_opensearch_version(ver)
    <<~EOM
      OPENSEARCH009: Invalid OpenSearch version detected
                     Chef Server currently supports Opensearch version #{SUPPORTED_VERSION}.
                     The OpenSearch instance running at #{search_engine_url} is version #{ver}.

                     Please check the documentation for more details about supported
                     versions: https://docs.chef.io/doc-page#anchor-to-version-info
      EOM
  end
end
