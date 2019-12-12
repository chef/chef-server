#
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

# NOTE:
#
# Uses the value of node['private_chef']['postgresql']['username'] as
# the user to run the database-creation psql command

action :create do
  EcPostgres.with_connection(node) do |connection|
    result = connection.exec("SELECT datname FROM pg_database WHERE datname='#{new_resource.database}'")
    if result.ntuples == 0
      converge_by("Create database #{new_resource.database}") do
        owner = "WITH OWNER #{new_resource.owner}" if new_resource.owner
        connection.exec("CREATE DATABASE \"#{new_resource.database}\" #{owner} TEMPLATE #{new_resource.template} ENCODING '#{new_resource.encoding}';")
      end
    end
  end
end
