
postgres = node['private_chef']['postgresql']
erchef = node['private_chef']['opscode-erchef']

private_chef_pg_user erchef['sql_user'] do
  password erchef['sql_password']
  superuser false
end

private_chef_pg_user erchef['sql_ro_user'] do
  password erchef['sql_ro_password']
  superuser false
end

private_chef_pg_database "opscode_chef" do
  owner erchef['sql_user']
  notifies :deploy, "private_chef_pg_sqitch[/opt/opscode/embedded/service/opscode-erchef/schema/baseline]", :immediately
end

# For existing installations, make sure the database owner is set to sql_user
ruby_block "set opscode_chef ownership" do
  block do
    EcPostgres.with_connection(node, 'opscode_chef') do |connection|
      connection.exec("ALTER DATABASE opscode_chef OWNER TO #{erchef['sql_user']};")
    end
  end
end

# Note that the sqitch migrations below only trigger when we create the database.
# At this time, we're using partybus to apply upgrade-related sqitch migrations,
# so that we can also apply any necessary data migrations (not yet managed through sqitch)
# at that time.
private_chef_pg_sqitch "/opt/opscode/embedded/service/opscode-erchef/schema/baseline" do
  hostname  postgres['vip']
  port      postgres['port']
  # TODO: will become db_superuser , db_superuser_password
  username  postgres['username']
  database  "opscode_chef"
  action :nothing
  notifies :deploy, "private_chef_pg_sqitch[/opt/opscode/embedded/service/opscode-erchef/schema]", :immediately
end

private_chef_pg_sqitch "/opt/opscode/embedded/service/opscode-erchef/schema" do
  hostname  postgres['vip']
  port      postgres['port']
  # TODO: will become db_superuser , db_superuser_password
  username  postgres['username']
  database "opscode_chef"
  action :nothing
end


private_chef_pg_user_table_access erchef['sql_user'] do
  database 'opscode_chef'
  schema 'public'
  access_profile :write
  only_if { is_data_master? }
end

private_chef_pg_user_table_access erchef['sql_ro_user'] do
  database 'opscode_chef'
  schema 'public'
  access_profile :read
  only_if { is_data_master? }
end

# Cleanup old enterprise-chef-server-schema
directory "/opt/opscode/embedded/service/enterprise-chef-server-schema" do
  recursive true
  action :delete
end
