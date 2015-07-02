<<<<<<< HEAD
private_chef_pg_user node['private_chef']['postgresql']['sql_user'] do
  password node['private_chef']['postgresql']['sql_password']
  superuser false
end

private_chef_pg_user node['private_chef']['postgresql']['sql_ro_user'] do
  password node['private_chef']['postgresql']['sql_ro_password']
  superuser false
end

private_chef_pg_database "opscode_chef" do
  owner node['private_chef']['postgresql']['sql_user']
  notifies :run, "execute[chef-server-schema]", :immediately
end
=======
postgres = node['private_chef']['postgresql']
>>>>>>> Adding pg_sqitch provider and updates to bifrost and erchef recipes to use it

private_chef_pg_user postgres['sql_user'] do
  password postgres['sql_password']
  superuser false
end
private_chef_pg_user postgres['sql_ro_user'] do
  password postgres['sql_ro_password']
  superuser false
end

private_chef_pg_database "opscode_chef" do
  owner postgres['sql_user']
  notifies :run, "private_chef_pg_sqitch[/opt/opscode/embedded/service/opscode-erchef/schema/baseline]", :immediately
end

# For existing installations, make sure the database owner is set to sql_user
ruby_block "set opscode_chef ownership" do
  block do
    EcPostgres.with_connection(node, 'opscode_chef') do |connection|
      connection.exec("ALTER DATABASE opscode_chef OWNER TO #{node['private_chef']['postgresql']['sql_user']};")
    end
  end
  
# TODO these originally were only run on notify - is this still required
# to get a sqitch schema in place from EC11? We already use sqitch in
# 11.2 and 11.3, which I think are the only ones we support upgrading from.
# Refer to master to setup notification based execution

private_chef_pg_sqitch "/opt/opscode/embedded/service/opscode-erchef/schema/baseline" do
  hostname  postgres['vip']
  port      postgres['port']
  username  postgres['sql_user']
  password  postgres['sql_password']
  database  "opscode_chef"
  action :nothing
  notifies :run, "private_chef_pg_sqitch[/opt/opscode/embedded/service/opscode-erchef/schema]", :immediately
>>>>>>> Adding pg_sqitch provider and updates to bifrost and erchef recipes to use it
end

private_chef_pg_sqitch "/opt/opscode/embedded/service/opscode-erchef/schema" do
  hostname  postgres['vip']
  port      postgres['port']
  username  postgres['sql_user']
  password  postgres['sql_password']
  database "opscode_chef"
  action :nothing
end


# TODO ... sqitch this.
private_chef_pg_user_table_access postgres['sql_ro_user'] do
  database 'opscode_chef'
  schema 'public'
  access_profile :read
end

# Cleanup old enterprise-chef-server-schema
# TODO don't we have a cleanup mechanism this can live in?
directory "/opt/opscode/embedded/service/enterprise-chef-server-schema" do
  recursive true
  action :delete
end
