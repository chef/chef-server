#
# Author:: Adam Jacob (<adam@chef.io>)
# Copyright:: Copyright (c) 2011 Opscode, Inc.
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

# Create a user for Chef services to run as
user OmnibusHelper.new(node).ownership['owner'] do
  system true
  shell node['private_chef']['user']['shell'] 
  home node['private_chef']['user']['home']
end

group OmnibusHelper.new(node).ownership['group'] do
  members [OmnibusHelper.new(node).ownership['owner']]
end
