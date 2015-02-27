private_chef_pg_database "opscode_chef" do
#  owner node['private_chef']['postgresql']['username'] # Do we really want this?
  notifies :run, "execute[chef-server-schema]", :immediately
end

# Though Sqitch runs are idempotent, we need to have them run only
# when the database is first created (thus the chained notifications).
# This is only for EC11; subsequent versions can run them
# idempotently, like normal.  This is just to allow fresh EC11
# installs to use Sqitch; upgraded installations have a Partybus
# upgrade to run.  The end result should be that whether or not you
# are installing or upgrading EC11, at the end of the day, you'll have
# Sqitch metadata info in your database.
execute "chef-server-schema" do
  command "sqitch --db-name opscode_chef deploy --verify"
  # OSC schema is a dependency of the EC schema, and managed by it
  cwd "/opt/opscode/embedded/service/opscode-erchef/schema/baseline"
  user node['private_chef']['postgresql']['username']
  # Clear PERL5LIB to ensure sqitch only uses omnibus's perl
  # installation
  environment "PERL5LIB" => ""
  returns [0,1]
  action :nothing
  notifies :run, "execute[enterprise-chef-server-schema]", :immediately
end

execute "enterprise-chef-server-schema" do
  command "sqitch --db-name opscode_chef deploy --verify"
  cwd "/opt/opscode/embedded/service/opscode-erchef/schema"
  user node['private_chef']['postgresql']['username']
  # Clear PERL5LIB to ensure sqitch only uses omnibus's perl
  # installation
  environment "PERL5LIB" => ""
  returns [0,1]
  action :nothing
end


# Create Database Users

private_chef_pg_user node['private_chef']['postgresql']['sql_user'] do
  password node['private_chef']['postgresql']['sql_password']
  superuser false
end

private_chef_pg_user_table_access node['private_chef']['postgresql']['sql_user'] do
  database 'opscode_chef'
  schema 'public'
  access_profile :write
end

private_chef_pg_user node['private_chef']['postgresql']['sql_ro_user'] do
  password node['private_chef']['postgresql']['sql_ro_password']
  superuser false
end

private_chef_pg_user_table_access node['private_chef']['postgresql']['sql_ro_user'] do
  database 'opscode_chef'
  schema 'public'
  access_profile :read
end

# Cleanup old enterprise-chef-server-schema
directory "/opt/opscode/embedded/service/enterprise-chef-server-schema" do
  recursive true
  action :delete
end
