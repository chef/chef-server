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


postgresql_dir = node['private_chef']['postgresql']['dir']
postgresql_data_dir = node['private_chef']['postgresql']['data_dir']
postgresql_data_dir_symlink = File.join(postgresql_dir, "data")
postgresql_log_dir = node['private_chef']['postgresql']['log_directory']

# Postgres User Setup
user node['private_chef']['postgresql']['username'] do
  system true
  shell node['private_chef']['postgresql']['shell']
  home node['private_chef']['postgresql']['home']
end

# TODO: Currently this is set up to be a parent directory of
# node['private_chef']['postgresql']['dir'].  Is it necessary that
# this is exposed as a settable attribute, or can we make some
# simplifying assumptions about our directory structure?
directory node['private_chef']['postgresql']['home'] do
  owner node['private_chef']['postgresql']['username']
  recursive true
  mode node['private_chef']['service_dir_perms']
end

file File.join(node['private_chef']['postgresql']['home'], ".profile") do
  owner node['private_chef']['postgresql']['username']
  mode "0644"
  content <<-EOH
    PATH=#{node['private_chef']['postgresql']['user_path']}
  EOH
end

####

directory postgresql_log_dir do
  owner OmnibusHelper.new(node).ownership['owner']
  group OmnibusHelper.new(node).ownership['group']
  recursive true
  mode node['private_chef']['service_dir_perms']
end

directory postgresql_dir do
  owner node['private_chef']['postgresql']['username']
  recursive true
  mode node['private_chef']['service_dir_perms']
end

# Upgrade the cluster if you gotta
private_chef_pg_upgrade "upgrade_if_necessary"

private_chef_pg_cluster postgresql_data_dir do
  notifies :restart, 'runit_service[postgresql]' if is_data_master?
end

link postgresql_data_dir_symlink do
  to postgresql_data_dir
  not_if { postgresql_data_dir == postgresql_data_dir_symlink }
end

component_runit_service "postgresql" do
  control ['t']
end

# NOTE: These recipes are written idempotently, but require a running
# PostgreSQL service.  They should run each time (on the appropriate
# backend machine, of course), because they also handle schema
# upgrades for new releases of Enterprise Chef.  As a result, we can't
# just do a check against node['private_chef']['bootstrap']['enable'],
# which would only run them one time.
if is_data_master?
  execute "/opt/opscode/bin/private-chef-ctl start postgresql" do
    retries 20
  end

  ruby_block "wait for postgresql to start" do
    block do
      connectable = false
      2.times do |i|
        # Note that we have to include the port even for a local pipe, because the port number
        # is included in the pipe default.
        `echo 'SELECT * FROM pg_database;' | su - #{node['private_chef']['postgresql']['username']} -c '/opt/opscode/embedded/bin/psql -p #{node['private_chef']['postgresql']['port']} -U #{node['private_chef']['postgresql']['db_superuser']} postgres -t -A'`
	if $?.exitstatus != 0
          Chef::Log.fatal("Could not connect to database, retrying in 10 seconds.")
          sleep 10
        else
          connectable = true
          break
        end
      end

      unless connectable
        Chef::Log.fatal <<-ERR

Could not connect to the postgresql database.
Please check 'chef-server-ctl tail postgresql' for more information.

ERR
        exit!(1)
      end
    end
  end

  # Update the postgresql superuser  with a password for tcp-based access.
  private_chef_pg_user node['private_chef']['postgresql']['db_superuser'] do
    password PrivateChef.credentials.get('postgresql', 'db_superuser_password')
    # This initial password set must be done over local socket:
    local_connection true
    # Don't make superuser into a non-superuser...
    superuser true
  end

  # Set up a database for the opscode-pgsql user to log into automatically
  private_chef_pg_database "opscode-pgsql"
  include_recipe "private-chef::erchef_database"
  include_recipe "private-chef::bifrost_database"
  include_recipe "private-chef::oc_id_database"
  include_recipe "private-chef::bookshelf_database" if node["private_chef"]["bookshelf"]["storage_type"] == "sql"
end
