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
  cwd "/opt/opscode/embedded/service/chef-server-schema"
  user node['private_chef']['postgresql']['username']
  returns [0,1]
  action :nothing
  notifies :run, "execute[enterprise-chef-server-schema]", :immediately
end

execute "enterprise-chef-server-schema" do
  command "sqitch --db-name opscode_chef deploy --verify"
  cwd "/opt/opscode/embedded/service/enterprise-chef-server-schema"
  user node['private_chef']['postgresql']['username']
  returns [0,1]
  action :nothing
end


# Create Database Users

# TODO: Originally these users were created WITH SUPERUSER... is that still necessary?
private_chef_pg_user node['private_chef']['postgresql']['sql_user'] do
  password node['private_chef']['postgresql']['sql_password']
  superuser true
  notifies :run, "execute[grant opscode_chef privileges]", :immediately
end

execute "grant opscode_chef privileges" do
  command <<-EOM.gsub(/\s+/," ").strip!
    psql --dbname opscode_chef
         --command "GRANT ALL PRIVILEGES ON DATABASE opscode_chef TO #{node['private_chef']['postgresql']['sql_user']};"
  EOM
  user node['private_chef']['postgresql']['username']
  action :nothing
end

private_chef_pg_user node['private_chef']['postgresql']['sql_ro_user'] do
  password node['private_chef']['postgresql']['sql_ro_password']
  superuser true
  notifies :run, "execute[grant opscode_chef_ro privileges]", :immediately
end

execute "grant opscode_chef_ro privileges" do
  command <<-EOM.gsub(/\s+/," ").strip!
    psql --dbname opscode_chef
         --command "GRANT ALL PRIVILEGES ON DATABASE opscode_chef TO #{node['private_chef']['postgresql']['sql_ro_user']};"
  EOM
  user node['private_chef']['postgresql']['username']
  action :nothing
end
