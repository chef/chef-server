private_chef_pg_database "opscode_chef" do
#  owner node['private_chef']['postgresql']['username'] # Do we really want this?
  notifies :run, "execute[migrate_database]", :immediately
end

# Note: as currently coded, this only runs the database migration when
# the database is first created.  Erchef does not currently use Sqitch
# (like Bifrost does), so subsequent schema upgrades are handled by
# the 'private-chef-ctl upgrade' mechanism.
execute "migrate_database" do
  command "bundle exec rake pg:remigrate"
  cwd "/opt/opscode/embedded/service/chef-sql-schema"
  user node['private_chef']['postgresql']['username']
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
