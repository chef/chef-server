#
# Author:: James Casey <james@chef.io>
# Copyright:: Copyright (c) 2014 Opscode, Inc.
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

id_attrs = node['private_chef']['oc_id']

# create users
private_chef_pg_user id_attrs['sql_user'] do
  password PrivateChef.credentials.get('oc_id', 'sql_password')
end

private_chef_pg_user id_attrs['sql_ro_user'] do
  password PrivateChef.credentials.get('oc_id', 'sql_ro_password')
end


private_chef_pg_database "oc_id" do
  owner id_attrs['sql_user']
end

private_chef_pg_user_table_access id_attrs['sql_ro_user'] do
  database 'oc_id'
  schema 'public'
  access_profile :read
  only_if { is_data_master? }
end

