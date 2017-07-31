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
  # The string field type has beein split into 'text' and 'keyword' for ES5
  # Generally, we will want text for partial matching content
  # (data field)
  # (data are separated out from 'string' in ES5+:
  if EsHelper.es_version(node) >= Gem::Version.new("5.0.0")
    text_field_type = "text"
    keyword_field_type = "keyword"
    puts "*** 5.+ detected"
  else
    keyword_field_type = "string"
    text_field_type = "string"
    puts "*** earlier version #{EsHelper.es_version(node)}"
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
                                                          "index" => "not_analyzed",
                                                          "norms" => {
                                                            "enabled" => false
                                                          }
                                                        },
                            "X_CHEF_type_CHEF_X" => { "type" => keyword_field_type,
                                                      "index" => "not_analyzed",
                                                      "norms" => {
                                                        "enabled" => false
                                                      }
                                                    },
                            "X_CHEF_id_CHEF_X" => { "type" => keyword_field_type,
                                                    "index" => "not_analyzed",
                                                    "norms" => {
                                                      "enabled" => false
                                                    }
                                                  },
                            "data_bag" => { "type" => keyword_field_type,
                                            "index" => "not_analyzed",
                                            "norms" => {
                                              "enabled" => false
                                            }
                                          },
                            "content" => { "type" => text_field_type, "index" => "analyzed"}
                          }
                        }
                      }
                     })
  end
end
