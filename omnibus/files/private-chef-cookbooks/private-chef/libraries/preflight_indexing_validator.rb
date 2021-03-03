#
# Copyright:: 2020 Chef Software, Inc.
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

require_relative './preflight_checks.rb'
require_relative './elasticsearch.rb'

class IndexingPreflightValidator < PreflightValidator
  # The cs_*attr variables hold the user-defined configuration
  attr_reader :cs_elasticsearch_attr, :cs_solr_attr, :cs_erchef_attr

  # The node_*attr variables hold the default configuration
  attr_reader :node_elasticsearch_attr, :node_erchef_attr

  def initialize(node)
    super
    @cs_elasticsearch_attr = PrivateChef['elasticsearch']
    @cs_solr_attr = PrivateChef['opscode_solr4']
    @cs_erchef_attr = PrivateChef['opscode_erchef']

    @node_elasticsearch_attr = node['private_chef']['elasticsearch']
    @node_erchef_attr = node['private_chef']['opscode-erchef']
  end

  def run!
    verify_system_memory
    verify_heap_size
    verify_disk_space
    verify_consistent_reindex_sleep_times
    verify_no_deprecated_indexing_options
    verify_es_disabled_if_user_set_external_solr
    verify_external_url
    verify_erchef_config
  end

  def verify_consistent_reindex_sleep_times
    final_min = cs_erchef_attr['reindex_sleep_min_ms'] || node_erchef_attr['reindex_sleep_min_ms']
    final_max = cs_erchef_attr['reindex_sleep_max_ms'] || node_erchef_attr['reindex_sleep_max_ms']
    if final_min > final_max
      fail_with err_INDEX001_failed_validation(final_min, final_max)
    end
  end

  def verify_no_deprecated_indexing_options
    fail_with err_INDEX002_failed_validation if PrivateChef['deprecated_solr_indexing']
  end

  # checks that system has atleast 4GB memory
  def verify_system_memory
    system_memory_mb = Elasticsearch.node_memory_in_units(node, :total, :mb)
    # Ideally our max would be 4192 but in a lot of virtualized
    # environments the OS ends up reporting much less than 4GB of RAM
    # to userspace even if the VM is started with 4GB.
    required_memory_mb = 3072
    if system_memory_mb < required_memory_mb
      fail_with err_INDEX003_insufficient_system_memory(system_memory_mb, required_memory_mb)
    end
  end

  # checks that system specifys a heap size between 1GB and 26GB
  def verify_heap_size
    es_heap_size = cs_elasticsearch_attr['heap_size'] || node_elasticsearch_attr['heap_size']
    solr_heap_size = cs_solr_attr['heap_size'] || 0

    using_sorl = false
    heap_size = if solr_heap_size > es_heap_size
                  using_solr = true
                  solr_heap_size
                else
                  es_heap_size
                end

    min_heap = 1024
    # https://www.elastic.co/guide/en/elasticsearch/reference/current/heap-size.html
    max_heap = 26 * 1024

    if heap_size < min_heap || heap_size > max_heap
      fail_with err_INDEX004_invalid_elasticsearch_heap_size(heap_size, using_solr)
    end
  end

  # Disk space verification for running reindex 
  # It needs enough disk space to hold a second copy of the data plus whatever change in size the new index might need if the schema has changed.
  # Required Disk Space = (2 * size of ES data directory)
  def verify_disk_space
    data_dir = node_elasticsearch_attr["data_dir"]
    if Dir.exist?(data_dir)
      data_dir_size = Du.du(data_dir)
      free_disk_space = Statfs.new("#{data_dir}/..").free_space
      if (2* data_dir_size) < free_disk_space #The minimum space should be double the existing data size
        Chef::Log.debug("Free space is sufficient to start Elasticsearch reindex")
      else
        Chef::Log.fatal("Insufficient free space on disk to complete reindex.")
        Chef::Log.fatal("The current elasticsearch data directory contains #{data_dir_size} KB of data but only #{free_disk_space} KB is available on disk.")
        Chef::Log.fatal("The reindex process requires at least #{2*data_dir_size} KB.")
        fail_with err_INDEX008_insufficient_disk_space(free_disk_space, 2*data_dir_size)
      end
    else
      Chef::Log.fatal("Elasticserach data path does not exist, so skipping the disk space preflight validations: #{data_dir}")
    end
  end

  # If an external search provider was explicitly enabled by the user,
  # then we expect that both internal search services should be
  # disabled.
  def verify_es_disabled_if_user_set_external_solr
    if external? && elasticsearch_enabled?
      fail_with err_INDEX005_should_dsiable_es
    end
  end

  def external?
    cs_elasticsearch_attr['external'] || cs_solr_attr['external']
  end

  def elasticsearch_enabled?
    return false if PrivateChef['use_chef_backend']

    if cs_elasticsearch_attr['enable'].nil?
      node_elasticsearch_attr['enable']
    else
      cs_elasticsearch_attr['enable']
    end
  end

  def verify_external_url
    if cs_elasticsearch_attr['external'] && !cs_elasticsearch_attr['external_url']
      fail_with err_INDEX006_bad_external_config(false)
    end
    if cs_solr_attr['external'] && !cs_solr_attr['external_url']
      fail_with err_INDEX006_bad_external_config(true)
    end
  end

  def verify_erchef_config
    provider = @cs_erchef_attr['search_provider']
    return true if provider.nil? # default provider

    unless %w(batch inline).include?(@cs_erchef_attr['search_queue_mode'])
      fail_with err_INDEX007_bad_queue_mode
    end
  end

  def err_INDEX001_failed_validation(final_min, final_max)
    <<~EOM

      INDEX001: opscode_erchef['reindex_sleep_min_ms'] (#{final_min}) is greater than
                opscode_erchef['reindex_sleep_max_ms'] (#{final_max})

                The maximum sleep time should be greater or equal to the minimum sleep
                time.
    EOM
  end

  def err_INDEX002_failed_validation
    <<~EOM
      INDEX002: Deprecated Solr Indexing

                You have configured

                    deprecated_solr_indexing true

                The Solr 4-based indexing pipeline is no longer supported.

                Please contact Chef Support for help moving to the
                Elasticsearch indexing pipeline.
    EOM
  end

  def err_INDEX003_insufficient_system_memory(system_memory, required_memory)
    <<~EOM

      INDEX003: Insufficient system memory

                System has #{system_memory} MB of memory,
                but #{required_memory} MB is required.

    EOM
  end

  def err_INDEX004_invalid_elasticsearch_heap_size(heap_size, using_solr)
    if using_solr
      <<~EOM
        INDEX004: Invalid elasticsearch heap size

                      opscode_solr4['heap_size'] is #{heap_size}MB

                  This value from the Solr search index configuration cannot
                  be safely used for the new Elasticsearch search index.

                  The recommended heap_size is between 1GB and 26GB. Refer to
                  https://www.elastic.co/guide/en/elasticsearch/reference/6.8/heap-size.html
                  for more information.

                  Consider removing this configuration and adding a new value
                  for
                        elasticsearch['heap_size']

                  within the allowed range to #{CHEF_SERVER_CONFIG_FILE}.

      EOM
    else
      <<~EOM
        INDEX004: Invalid elasticsearch heap size

                      elasticsearch['heap_size'] is #{heap_size}MB

                  The recommended heap_size is between 1GB and 26GB. Refer to
                  https://www.elastic.co/guide/en/elasticsearch/reference/6.8/heap-size.html
                  for more information.
      EOM
    end
  end

  def err_INDEX005_should_disable_es
    <<~EOM

      INDEX005: The #{CHEF_SERVER_NAME} is configured to use an external search
                index but the internal Elasticsearch is still enabled. This is
                an unsupported configuration.

                To disable the internal Elasticsearch, add the following to #{CHEF_SERVER_CONFIG_FILE}:

                    elasticsearch['enable'] = false

                To use the internal Elasticsearch, consider removing the configuration
                entry for opscode_solr4['external'].

     EOM
  end

  def err_INDEX006_bad_external_config(was_solr)
    if was_solr
      <<~EOM

        INDEX006: No external url specified for Elasticsearch depsite opscode_solr4['external']
                  being set to true.

                  To use an external Elasticsearch instance, please set:

                      elasticsearch['external'] = true
                      elasticsearch['external_url'] = YOUR_ELASTICSEARCH_URL

                  in #{CHEF_SERVER_CONFIG_FILE}
       EOM
    else
      <<~EOM

        INDEX006: No external url specified for Elasticsearch depsite elasticsearch['external']
                  being set to true.

                  To use an external Elasticsearch instance, please set:

                      elasticsearch['external'] = true
                      elasticsearch['external_url'] = YOUR_ELASTICSEARCH_URL

                  in #{CHEF_SERVER_CONFIG_FILE}
      EOM
    end
  end

  def err_INDEX007_bad_queue_mode
    <<~EOM

      INDEX007: The elasticsearch provider is only supported by the batch or inline
                queue modes. To use the elasticsearch provider, please also set:

                    opscode_erchef['search_queue_mode'] = 'batch'

                in #{CHEF_SERVER_CONFIG_FILE}
    EOM
  end

  def err_INDEX008_insufficient_disk_space(system_space, required_space)
    <<~EOM

      INDEX003: Insufficient disk space

                System has #{system_space} MB of space,
                but #{required_space} MB is required.

    EOM
  end
end
