# Author:: Marc Paradise <marc@chef.io>
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

# External installations require only that we set up our application databases.
if is_data_master? and node['private_chef']['postgresql']['external']
  include_recipe "private-chef::erchef_database"
  include_recipe "private-chef::bifrost_database"
  include_recipe "private-chef::oc_id_database"
  include_recipe "private-chef::bookshelf_database" if node["private_chef"]["bookshelf"]["storage_type"].to_s == "sql"
end
