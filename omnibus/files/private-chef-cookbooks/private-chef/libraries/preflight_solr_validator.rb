#
# Copyright:: Copyright (c) 2015 Chef Software, Inc.
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
class SolrPreflightValidator < PreflightValidator
  attr_reader :cs_solr_attr, :node_solr_attr, :cs_erchef_attr

  def initialize(node)
    super
    @cs_solr_attr = PrivateChef['opscode_solr4']
    @node_solr_attr = node['private_chef']['opscode-solr4']
    @cs_erchef_attr = PrivateChef['opscode_erchef']
  end

  def run!
    warn_unchanged_external_flag
    verify_external_url
    verify_erchef_config
  end

  def warn_unchanged_external_flag
    if OmnibusHelper.has_been_bootstrapped? && backend?  && previous_run
      if cs_solr_attr.has_key?('external') && (cs_solr_attr['external'] != previous_run['opscode-solr4']['external'])
        Chef::Log.warn <<-EOM

The value of opscode_solr4['external'] has been changed.  Search
results against the new external search index may be incorrect. Please
run `chef-server-ctl reindex --all` to ensure correct results

EOM
      end
    else
      return true
    end
  end

  def verify_external_url
    if cs_solr_attr['external'] & (! cs_solr_attr['external_url'])
      fail_with <<EOM

No external url specified for Solr depsite opscode_solr4['external']
being set to true. To use an external solr instance, please set
opscode_solr4['external_url'] to the external solr endpoint.

EOM
    end
  end

  def verify_erchef_config
    provider = @cs_erchef_attr['search_provider']
    return true if provider.nil? #default provider
    case provider
    when 'elasticsearch'
      if ! ['batch', 'inline'].include?(@cs_erchef_attr['search_queue_mode'])
        fail_with <<-EOM

The elasticsearch provider is only supported by the batch or inline
queue modes. To use the elasticsearch provider, please also set:

opscode_erchef['search_queue_mode'] = 'batch'

in /etc/opscode/chef-server.rb

EOM
      end
    when 'solr'
    else
      fail_with <<-EOM
The specified search provider (#{provider}) is not currently supported.
Please choose from one of the following search providers:

solr
elasticsearch
EOM
    end
  end
end
