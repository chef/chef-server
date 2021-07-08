#
# Copyright:: 2017-2018 Chef Software, Inc.
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

# Remove old PostgreSQL data directory
#
# pg_upgrade will helpfully generate a cleanup script which removes
# the previous database cluster.  This is nice, because we don't have
# to worry about keeping track of the location of the old cluster.
execute 'remove_old_postgres_data_directory' do
  cleanup_script = "#{node['private_chef']['postgresql']['data_dir']}/delete_old_cluster.sh"
  command cleanup_script
  only_if { ::File.exist? cleanup_script }
  # The script itself is idempotent; it just runs an 'rm -Rf' on the
  # old $PG_DATA directory
end

directory '/var/log/opscode/postgresql/9.2' do
  recursive true
  action :delete
end
