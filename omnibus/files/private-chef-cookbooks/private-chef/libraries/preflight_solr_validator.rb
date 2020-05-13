#
# Copyright:: 2015-2018 Chef Software, Inc.
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

class SolrPreflightValidator < PreflightValidator
  attr_reader :cs_solr_attr, :node_solr_attr, :cs_erchef_attr, :node_erchef_attr

  def initialize(node)
    super
    @cs_solr_attr = PrivateChef['opscode_solr4']
    @cs_erchef_attr = PrivateChef['opscode_erchef']
    @cs_elasticsearch_attr = PrivateChef['elasticsearch']
    @cs_rabbitmq_attr = PrivateChef['rabbitmq']
    @cs_opscode_expander_attr = PrivateChef['opscode-expander']
    @node_erchef_attr = node['private_chef']['opscode-erchef']
    @node_solr_attr = node['private_chef']['opscode-solr4']
  end

  def run!
    verify_sane_reindex_sleep_times
    verify_es_disabled_if_user_set_external_solri
    verify_unused_services_are_disabled_if_using_internal_es
    warn_unchanged_external_flag
    verify_external_url
    verify_erchef_config
  end

  def verify_sane_reindex_sleep_times
    final_min = cs_erchef_attr['reindex_sleep_min_ms'] || node_erchef_attr['reindex_sleep_min_ms']
    final_max = cs_erchef_attr['reindex_sleep_max_ms'] || node_erchef_attr['reindex_sleep_max_ms']
    if final_min > final_max
      fail_with <<~EOM

        opscode_erchef['reindex_sleep_min_ms'] (#{final_min}) is greater than
        opscode_erchef['reindex_sleep_max_ms'] (#{final_max})

        The maximum sleep time should be greater or equal to the minimum sleep
        time.
      EOM
    end
  end

  # External_solr could point to either of es or solr
  # To test: installstandalone with es then upgrade to external es (or solr)
  def verify_es_disabled_if_user_set_external_solr
    if cs_solr_attr['external'] && cs_elasticsearch_attr['enable']
      fail_with <<~EOM

      You have configured an external search provider
      but have not disabled the built-in Elasticsearch. Please add the
      following to your configuration file:

          elasticsearch['enable'] = false

      EOM
    end
  end

  # The user might have either or a combination of solr, rabbitmq
  # or opscode-expander enabled in config.
  # This needs to be disabled to install internal es
  # To test: setup standalone cs with 13.2.0 add
  # opscode_solr4['enable'] = true
  # rabbitmq['enable'] = true
  # opscode_expander['enable'] = true to chef-server.rb
  # then upgrade to 13.3
  def verify_unused_services_are_disabled_if_using_internal_es
    if !cs_solr_attr['external'] && cs_solr_attr['enable']
      # Should we fail here or a warning is sufficient?
      Chef::Log.warn <<~EOM

      This build will install elasticsearch for search.
      But you have set opscode_solr4['enable'] = true
      in your config. Please update your config file to
      set that to false:

          opscode_solr4['enable'] = false

      EOM

      if !cs_solr_attr['external'] && cs_rabbitmq_attr['enable']
      # Should we fail here or a warning is sufficient?
      Chef::Log.warn <<~EOM

      This build will install elasticsearch for search.
      But you have set rabbitmq['enable'] = true
      in your config. Please update your config file to
      set that to false:

          rabbitmq['enable'] = false

      EOM

      if !cs_solr_attr['external'] && cs_opscode_expander_attr['enable']
      # Should we fail here or a warning is sufficient?
      Chef::Log.warn <<~EOM

      This build will install elasticsearch for search.
      But you have set opscode_expander['enable'] = true
      in your config. Please update your config file to
      set that to false:

          opscode_expander['enable'] = false

      EOM
  end

  def warn_unchanged_external_flag
    if OmnibusHelper.has_been_bootstrapped? && backend? && previous_run
      if cs_solr_attr.key?('external') && (cs_solr_attr['external'] != previous_run['opscode-solr4']['external'])
        Chef::Log.warn <<~EOM

          The value of opscode_solr4['external'] has been changed.  Search
          results against the new external search index may be incorrect. Please
          run `chef-server-ctl reindex --all` to ensure correct results

        EOM
      end
    else
      true
    end
  end

  def verify_external_url
    if cs_solr_attr['external'] & !cs_solr_attr['external_url']
      fail_with <<~EOM

        No external url specified for Solr depsite opscode_solr4['external']
        being set to true. To use an external solr instance, please set
        opscode_solr4['external_url'] to the external solr endpoint.

      EOM
    end
  end

  def verify_erchef_config
    provider = @cs_erchef_attr['search_provider']
    return true if provider.nil? # default provider

    case provider
    when 'elasticsearch'
      unless %w(batch inline).include?(@cs_erchef_attr['search_queue_mode'])
        fail_with <<~EOM

          The elasticsearch provider is only supported by the batch or inline
          queue modes. To use the elasticsearch provider, please also set:

          opscode_erchef['search_queue_mode'] = 'batch'

          in /etc/opscode/chef-server.rb

        EOM
      end
    when 'solr'
    else
      fail_with <<~EOM
        The specified search provider (#{provider}) is not currently supported.
        Please choose from one of the following search providers:

        solr
        elasticsearch
      EOM
    end
  end
end
