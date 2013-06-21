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

# BEGIN POSTGRES CLEANUP
#
# private-chef 2.0.0+ renamed postges service to postgresql, we need to cleanup
# all traces of the previously-named service
#
execute "/opt/opscode/bin/private-chef-ctl stop postgres" do
  retries 20
end

runit_service "postgres" do
  action :disable
end

directory "/opt/opscode/sv/postgres" do
  action :delete
  recursive true
end
# END POSTGRES CLEANUP

postgresql_dir = node['private_chef']['postgresql']['dir']
postgresql_data_dir = node['private_chef']['postgresql']['data_dir']
postgresql_data_dir_symlink = File.join(postgresql_dir, "data")
postgresql_log_dir = node['private_chef']['postgresql']['log_directory']

user node['private_chef']['postgresql']['username'] do
  system true
  shell node['private_chef']['postgresql']['shell']
  home node['private_chef']['postgresql']['home']
end

directory postgresql_log_dir do
  owner node['private_chef']['user']['username']
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

file File.join(node['private_chef']['postgresql']['home'], ".profile") do
  owner node['private_chef']['postgresql']['username']
  mode "0644"
  content <<-EOH
PATH=#{node['private_chef']['postgresql']['user_path']}
EOH
end

if File.directory?("/etc/sysctl.d") && File.exists?("/etc/init.d/procps")
  # smells like ubuntu...
  service "procps" do
    action :nothing
  end

  template "/etc/sysctl.d/90-postgresql.conf" do
    source "90-postgresql.conf.sysctl.erb"
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

execute "/opt/opscode/embedded/bin/initdb -D #{postgresql_data_dir}" do
  user node['private_chef']['postgresql']['username']
  not_if { File.exists?(File.join(postgresql_data_dir, "PG_VERSION")) }
end

postgresql_config = File.join(postgresql_data_dir, "postgresql.conf")

template postgresql_config do
  source "postgresql.conf.erb"
  owner node['private_chef']['postgresql']['username']
  mode "0644"
  variables(node['private_chef']['postgresql'].to_hash)
  notifies :restart, 'service[postgresql]' if OmnibusHelper.should_notify?("postgresql")
end

pg_hba_config = File.join(postgresql_data_dir, "pg_hba.conf")

template pg_hba_config do
  source "pg_hba.conf.erb"
  owner node['private_chef']['postgresql']['username']
  mode "0644"
  variables(node['private_chef']['postgresql'].to_hash)
  notifies :restart, 'service[postgresql]' if OmnibusHelper.should_notify?("postgresql")
end

should_notify = OmnibusHelper.should_notify?("postgresql")

runit_service "postgresql" do
  down node['private_chef']['postgresql']['ha']
  control(['t'])
  options({
    :log_directory => postgresql_log_dir,
    :svlogd_size => node['private_chef']['postgresql']['log_rotation']['file_maxbytes'],
    :svlogd_num  => node['private_chef']['postgresql']['log_rotation']['num_to_keep']
  }.merge(params))
end

if node['private_chef']['bootstrap']['enable']
  execute "/opt/opscode/bin/private-chef-ctl start postgresql" do
    retries 20
  end

  # Create all required databases, migrate them, create the users we need, and grant them
  # privileges.

  #
  # oc_erchef
  #
  erchef_database_exists = "/opt/opscode/embedded/bin/chpst -u #{node['private_chef']['postgresql']['username']} /opt/opscode/embedded/bin/psql -d 'template1' -c 'select datname from pg_database' -x|grep opscode_chef"
  erchef_user_exists     = "/opt/opscode/embedded/bin/chpst -u #{node['private_chef']['postgresql']['username']} /opt/opscode/embedded/bin/psql -d 'template1' -c 'select usename from pg_user' -x|grep #{node['private_chef']['postgresql']['sql_user']}"
  erchef_ro_user_exists  = "/opt/opscode/embedded/bin/chpst -u #{node['private_chef']['postgresql']['username']} /opt/opscode/embedded/bin/psql -d 'template1' -c 'select usename from pg_user' -x|grep #{node['private_chef']['postgresql']['sql_ro_user']}"

  execute "/opt/opscode/embedded/bin/createdb -T template0 -E UTF-8 opscode_chef" do
    user node['private_chef']['postgresql']['username']
    not_if erchef_database_exists
    retries 30
    notifies :run, "execute[migrate_database]", :immediately
  end

  execute "migrate_database" do
    command "/opt/opscode/embedded/bin/bundle exec /opt/opscode/embedded/bin/rake pg:remigrate"
    cwd "/opt/opscode/embedded/service/chef-sql-schema"
    user node['private_chef']['postgresql']['username']
    action :nothing
  end

  execute "/opt/opscode/embedded/bin/psql -d 'opscode_chef' -c \"CREATE USER #{node['private_chef']['postgresql']['sql_user']} WITH SUPERUSER ENCRYPTED PASSWORD '#{node['private_chef']['postgresql']['sql_password']}'\"" do
    cwd "/opt/opscode/embedded/service/chef-sql-schema"
    user node['private_chef']['postgresql']['username']
    notifies :run, "execute[grant opscode_chef privileges]", :immediately
    not_if erchef_user_exists
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
    not_if erchef_ro_user_exists
  end

  execute "grant opscode_chef_ro privileges" do
    command "/opt/opscode/embedded/bin/psql -d 'opscode_chef' -c \"GRANT ALL PRIVILEGES ON DATABASE opscode_chef TO #{node['private_chef']['postgresql']['sql_ro_user']}\""
    user node['private_chef']['postgresql']['username']
    action :nothing
  end

  #
  # oc_bifrost
  #
  # Bifrost uses a separate database running under the same PostgreSQL service.
  # Although it may make sense to perform this bootstrapping in the
  # `private-chef::bifrost` recipe (or it's own recipe) we've left it here
  # to follow the idioms of the rest of this cookbook. This should make a
  # cookbooks-wide refactor easier and allow cut down on the tribal knowledge
  # required to get a new engineer up to speed.
  #

  bifrost_database_exists = <<-EOH
    /opt/opscode/embedded/bin/chpst -u #{node['private_chef']['postgresql']['username']} \
    /opt/opscode/embedded/bin/psql --dbname template1 \
                                   --command "SELECT datname FROM pg_database" \
                                   -x \
    |grep bifrost
  EOH

  bifrost_user_exists = <<-EOH
    /opt/opscode/embedded/bin/chpst -u #{node['private_chef']['postgresql']['username']} \
    /opt/opscode/embedded/bin/psql --dbname template1 \
                                   --tuples-only \
                                   --command "SELECT rolname FROM pg_roles WHERE rolname='#{node['private_chef']['bifrost']['sql_user']}';" \
    | grep #{node['private_chef']['bifrost']['sql_user']}
  EOH

  bifrost_ro_user_exists = <<-EOH
    /opt/opscode/embedded/bin/chpst -u #{node['private_chef']['postgresql']['username']} \
    /opt/opscode/embedded/bin/psql --dbname template1 \
                                   --tuples-only \
                                   --command "SELECT rolname FROM pg_roles WHERE rolname='#{node['private_chef']['bifrost']['sql_ro_user']}';" \
    | grep #{node['private_chef']['bifrost']['sql_ro_user']}
  EOH

  # create users
  execute "create_db_user_bifrost" do
    command <<-EOH
      /opt/opscode/embedded/bin/psql --dbname template1 \
                                     --command "CREATE ROLE #{node['private_chef']['bifrost']['sql_user']} WITH LOGIN ENCRYPTED PASSWORD '#{node['private_chef']['bifrost']['sql_password']}'"
    EOH
    user node['private_chef']['postgresql']['username']
    not_if bifrost_user_exists
  end
  execute "create_db_ro_user_bifrost" do
    command <<-EOH
      /opt/opscode/embedded/bin/psql --dbname template1 \
                                     --command "CREATE ROLE #{node['private_chef']['bifrost']['sql_ro_user']} WITH LOGIN ENCRYPTED PASSWORD '#{node['private_chef']['bifrost']['sql_ro_password']}'"
    EOH
    user node['private_chef']['postgresql']['username']
    not_if bifrost_ro_user_exists
  end

  # create database
  execute "create_database_bifrost" do
    command <<-EOH
      /opt/opscode/embedded/bin/createdb --template template0 \
                                         --encoding UTF-8 \
                                         --owner #{node['private_chef']['bifrost']['sql_user']} \
                                         bifrost
    EOH
    user node['private_chef']['postgresql']['username']
    not_if bifrost_database_exists
    retries 30
    notifies :run, "execute[migrate_database_bifrost]", :immediately
    notifies :run, "execute[add_permissions_bifrost]", :immediately
  end

  # Currently this just does an install from scratch.  Eventually we
  # will adopt a migration approach.  A front-runner is Sqitch
  # (https://github.com/theory/sqitch), from the creator of pgTAP.
  execute "migrate_database_bifrost" do
    command "psql -d bifrost --set ON_ERROR_STOP=1 --single-transaction -f bifrost.sql"
    cwd "/opt/opscode/embedded/service/bifrost/db"
    user node['private_chef']['postgresql']['username']

    # Once we're using proper migrations, we can just have this action
    # execute automatically, instead of being triggered only when a new
    # database is created
    action :nothing
  end

  # Permissions for the database users got set in the schema... though that means that the role names should be hard-coded in this cookbook.
  execute "add_permissions_bifrost" do
    command <<-EOH
      psql --dbname bifrost \
           --single-transaction \
           --set ON_ERROR_STOP=1 \
           --set database_name=bifrost \
           --file permissions.sql
    EOH
    cwd "/opt/opscode/embedded/service/bifrost/db"
    user node['private_chef']['postgresql']['username']

    # Eventually, this will probably just be part of the migration
    action :nothing
  end

end

add_nagios_hostgroup("postgresql")
