#
# Author:: Adam Jacob (<adam@opscode.com>)
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

user node['private_chef']['postgresql']['username'] do
  system true
  shell node['private_chef']['postgresql']['shell']
  home node['private_chef']['postgresql']['dir']
end

directory postgresql_log_dir do
  owner node['private_chef']['postgresql']['username']
  recursive true
end

directory postgresql_dir do
  owner node['private_chef']['postgresql']['username']
  mode "0700"
end

directory postgresql_data_dir do
  owner node['private_chef']['postgresql']['username']
  mode "0700"
  recursive true
end

link postgresql_data_dir_symlink do
  to postgresql_data_dir
  not_if { postgresql_data_dir == postgresql_data_dir_symlink }
end

if File.directory?("/etc/sysctl.d") && File.exists?("/etc/init.d/procps")
  # smells like ubuntu...
  service "procps" do
    action :nothing
  end

  template "/etc/sysctl.d/90-postgres.conf" do
    source "90-postgres.conf.sysctl.erb"
    owner "root"
    mode  "0644"
    variables(node['private_chef']['postgresql'].to_hash)
    notifies :start, 'service[procps]', :immediately
  end
else
  # hope this works...
  execute "sysctl" do
    command "/sbin/sysctl -p /etc/sysctl.conf"
    action :nothing
  end

  # this is why i want cfengine-style editfile resources with appendifnosuchline, etc...
  bash "add shm settings" do
    user "root"
    code <<-EOF
      echo 'kernel.shmmax = 17179869184' >> /etc/sysctl.conf
      echo 'kernel.shmall = 4194304' >> /etc/sysctl.conf
    EOF
    notifies :run, 'execute[sysctl]', :immediately
    not_if "egrep '^kernel.shmmax = ' /etc/sysctl.conf"
  end
end


if node['private_chef']['bootstrap']['enable']
  execute "/opt/opscode/embedded/bin/initdb -D #{postgresql_data_dir}" do
    user node['private_chef']['postgresql']['username']
    not_if { File.exists?(File.join(postgresql_data_dir, "PG_VERSION")) }
  end
end

postgresql_config = File.join(postgresql_data_dir, "postgresql.conf")

template postgresql_config do
  source "postgresql.conf.erb"
  owner node['private_chef']['postgresql']['username']
  mode "0644"
  variables(node['private_chef']['postgresql'].to_hash)
  notifies :restart, 'service[postgres]' if OmnibusHelper.should_notify?("postgres")
end

pg_hba_config = File.join(postgresql_data_dir, "pg_hba.conf")

template pg_hba_config do
  source "pg_hba.conf.erb"
  owner node['private_chef']['postgresql']['username']
  mode "0644"
  variables(node['private_chef']['postgresql'].to_hash)
  notifies :restart, 'service[postgres]' if OmnibusHelper.should_notify?("postgres")
end

should_notify = OmnibusHelper.should_notify?("postgres")

runit_service "postgres" do
  down node['private_chef']['postgresql']['ha']
  control(['t'])
  options({
    :log_directory => postgresql_log_dir,
    :svlogd_size => node['private_chef']['postgresql']['svlogd_size'],
    :svlogd_num  => node['private_chef']['postgresql']['svlogd_num']
  }.merge(params))
end

if node['private_chef']['bootstrap']['enable'] 
	execute "/opt/opscode/bin/private-chef-ctl postgres start" do
		retries 20 
	end

  ###
  # Create the database, migrate it, and create the users we need, and grant them 
  # privileges.
  ###
  database_exists = "/opt/opscode/embedded/bin/chpst -u #{node['private_chef']['postgresql']['username']} /opt/opscode/embedded/bin/psql -d 'template1' -c 'select datname from pg_database' -x|grep opscode_chef"
  user_exists     = "/opt/opscode/embedded/bin/chpst -u #{node['private_chef']['postgresql']['username']} /opt/opscode/embedded/bin/psql -d 'template1' -c 'select usename from pg_user' -x|grep #{node['private_chef']['postgresql']['sql_user']}"
  ro_user_exists  = "/opt/opscode/embedded/bin/chpst -u #{node['private_chef']['postgresql']['username']} /opt/opscode/embedded/bin/psql -d 'template1' -c 'select usename from pg_user' -x|grep #{node['private_chef']['postgresql']['sql_ro_user']}"

  execute "/opt/opscode/embedded/bin/createdb -T template0 -E UTF-8 opscode_chef" do
    user node['private_chef']['postgresql']['username']
    not_if database_exists
    retries 30
    notifies :run, "execute[migrate_database]", :immediately
    notifies :run, "execute[migrate_reporting_database]", :immediately
  end

  execute "migrate_database" do
    command "/opt/opscode/embedded/bin/bundle exec /opt/opscode/embedded/bin/rake pg:remigrate" 
    cwd "/opt/opscode/embedded/service/chef-sql-schema"
    user node['private_chef']['postgresql']['username']
    action :nothing
  end

  #
  # Ultimately reporting service will have its own database. Until
  # then we need to manage its migrations seperately.
  # TODO: Remove this migration when reporting is moved to its own db.
  #
  execute "migrate_reporting_database" do
    command "/opt/opscode/embedded/bin/bundle exec /opt/opscode/embedded/bin/rake pg:remigrate" 
    cwd "/opt/opscode/embedded/service/opscode-reporting/db"
    user node['private_chef']['postgresql']['username']
    action :nothing
  end

  execute "/opt/opscode/embedded/bin/psql -d 'opscode_chef' -c \"CREATE USER #{node['private_chef']['postgresql']['sql_user']} WITH SUPERUSER ENCRYPTED PASSWORD '#{node['private_chef']['postgresql']['sql_password']}'\"" do
    cwd "/opt/opscode/embedded/service/chef-sql-schema"
    user node['private_chef']['postgresql']['username']
    notifies :run, "execute[grant opscode_chef privileges]", :immediately
    not_if user_exists
  end

  execute "grant opscode_chef privileges" do
    command "/opt/opscode/embedded/bin/psql -d 'opscode_chef' -c \"GRANT ALL PRIVILEGES ON DATABASE opscode_chef TO #{node['private_chef']['postgresql']['sql_user']}\"" 
    user node['private_chef']['postgresql']['username']
    action :nothing
  end

  execute "/opt/opscode/embedded/bin/psql -d 'opscode_chef' -c \"CREATE USER #{node['private_chef']['postgresql']['sql_ro_user']} WITH SUPERUSER ENCRYPTED PASSWORD '#{node['private_chef']['postgresql']['sql_ro_password']}'\"" do
    cwd "/opt/opscode/embedded/service/chef-sql-schema"
    user node['private_chef']['postgresql']['username']
    notifies :run, "execute[grant opscode_chef_ro privileges]", :immediately
    not_if ro_user_exists
  end

  execute "grant opscode_chef_ro privileges" do
    command "/opt/opscode/embedded/bin/psql -d 'opscode_chef' -c \"GRANT ALL PRIVILEGES ON DATABASE opscode_chef TO #{node['private_chef']['postgresql']['sql_ro_user']}\"" 
    user node['private_chef']['postgresql']['username']
    action :nothing
  end
end

add_nagios_hostgroup("postgresql")

