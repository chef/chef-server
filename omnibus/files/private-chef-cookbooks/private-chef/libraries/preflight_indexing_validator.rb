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

class IndexingPreflightValidator < PreflightValidator
  # The cs_*attr variables hold the user-defined configuration
  attr_reader :cs_erchef_attr

  # The node_*attr variables hold the default configuration
  attr_reader :node_erchef_attr

  def initialize(node)
    super

    @cs_erchef_attr = PrivateChef['opscode_erchef']
    @node_erchef_attr = node['private_chef']['opscode-erchef']
  end

  def run!
    verify_consistent_reindex_sleep_times
    verify_no_deprecated_indexing_options
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
      INDEX001: You have configured

                  deprecated_solr_indexing true

               The Solr 4-based indexing pipeline is no longer supported.

               Please contact Chef Support for help moving to the
               Elasticsearch indexing pipeline.
    EOM
  end
end
