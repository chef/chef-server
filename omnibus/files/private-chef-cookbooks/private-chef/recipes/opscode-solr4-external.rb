# Author:: Steven Danna
# Copyright:: Copyright (c) 2015 Chef Software, Inc
# License:: Apache License, Version 2.0
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
#


case node['private_chef']['opscode-erchef']['search_provider']
when 'solr'
  Chef::Log.warn("External Solr Support does not include configuring the Solr schema.")
when 'elasticsearch'
  # Several fields have changed from ES2->ES5 - this segment
  # maps those differences.
  #
  # In addition the string field type has beein
  # split into 'text' and 'keyword' for ES5.  We
  # we will want text if we need partial matches (as for the 'data' field)
  #  and 'keyword' for all non-analyzed fields.
  if EsHelper.es_version(node) >= Gem::Version.new("5.0.0")
    text_field_type = "text"
    keyword_field_type = "keyword"
    analyzed = true
    not_analyzed = false
    disabled_norms = false
  else
    keyword_field_type = "string"
    text_field_type = "string"
    analyzed = "analyzed"
    not_analyzed = "not_analyzed"
    disabled_norms = { "enabled" => false }
  end

  elasticsearch_index "chef" do
    server_url node['private_chef']['opscode-solr4']['external_url']
    index_definition({"settings" => {
      "analysis" => {
        "analyzer" => {
          "default" => {
            "type" => "whitespace"
          }
        }
      },
      "number_of_shards" => node['private_chef']['opscode-solr4']['elasticsearch_shard_count'],
      "number_of_replicas" => node['private_chef']['opscode-solr4']['elasticsearch_replica_count']
    },
    "mappings" => {
      "object" => {
        "_source" => { "enabled" => false },
        "_all" => { "enabled" => false },
        "properties" => {
          "X_CHEF_database_CHEF_X" => { "type" => keyword_field_type,
                                        "index" => not_analyzed,
                                        "norms" => disabled_norms
        },
        "X_CHEF_type_CHEF_X" => { "type" => keyword_field_type,
                                  "index" => not_analyzed,
                                  "norms" => disabled_norms
        },
        "X_CHEF_id_CHEF_X" => { "type" => keyword_field_type,
                                "index" => not_analyzed,
                                "norms" => disabled_norms
        },
        "data_bag" => { "type" => keyword_field_type,
                        "index" => not_analyzed,
                        "norms" => disabled_norms
        },
        "content" => { "type" => text_field_type,
                       "index" => analyzed}
        }
      }
    }
    })
  end
end
