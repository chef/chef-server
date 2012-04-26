directory "/var/opt/opscode/upgrades" do
  mode   "0755"
  owner  "root"
  group  "root"
  recursive true
  action :create
end

# set the db connection string
db_type     = node['private_chef']['database_type']
db_protocol = db_type == "postgresql" ? "postgres" : "mysql2"
db_user     = node['private_chef'][db_type]['sql_user']
db_password = node['private_chef'][db_type]['sql_password']
db_vip      = node['private_chef'][db_type]['vip']
db_name     = "opscode_chef"

db_connection_string = "#{db_protocol}://#{db_user}:#{db_password}@#{db_vip}/#{db_name}"

# set the node role
node_role = node['private_chef']['role']

template "/opt/opscode/embedded/service/partybus/config.rb" do
  source "partybus_config.rb.erb"
  owner  "root"
  owner  "root"
  mode   "0644"
  variables(:connection_string => db_connection_string,
            :node_role => node_role)
end
