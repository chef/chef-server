#
# Copyright:: 2019 Chef Software, Inc.
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

class ElasticsearchPreflightValidator < PreflightValidator
  attr_reader :cs_es_attr, :node_es_attr

  def initialize(node)
    super
    @cs_es_attr = PrivateChef['elasticsearch']
    @node_es_attr = node['private_chef']['elasticsearch']
  end

  def run!
    verify_memory
    verify_sane_heap_size
  end

  #checks that system has atleast 4GB memory
  def verify_memory
    system_required_memory_in_gb = 4 #GB
    node[:memory][:total] =~ /^(\d+)kB/
    memory = $1
    if (memory.match(/mb/i) && memory.to_i < system_required_memory_in_gb * 1024) ||
       (memory.match(/kb/i) && memory.to_i < system_required_memory_in_gb * 1024 * 1024)
      fail_with <<-EOM

node[:memory][:total] is less #{memory.to_i}kB.
The minimum memory requirement for a performant system is 4GB.

      EOM
    end
  end

  #checks that system specifys a heap size between 1GB and 32GB
  def verify_sane_heap_size
    heap_size = @cs_es_attr['heap_size'] || @node_es_attr['heap_size']
    min_heap = 1024
    # https://www.elastic.co/guide/en/elasticsearch/reference/current/heap-size.html
    max_heap = 28 * 1024
    if (heap_size < min_heap && heap_size > max_heap)
      fail_with <<-EOM

elasticsearch['heap_size'] is #{heap_size}GB.
The minimum heap_size requirement for a preformant system is between 1GB and 28GB.
Refer to https://www.elastic.co/guide/en/elasticsearch/reference/6.8/heap-size.html
for max size information.

      EOM
    end
  end
end
