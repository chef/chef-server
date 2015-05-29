# This recipe assumes that the database server has already been set up
# and is running.

# Extract the attribute hash here so we're not quite so verbose
bifrost_attrs = node['private_chef']['oc_bifrost']
postgres_attrs = node['private_chef']['postgresql']

# create users
private_chef_pg_user bifrost_attrs['sql_user'] do
  password bifrost_attrs['sql_password']
  superuser false
end



private_chef_pg_user bifrost_attrs['sql_ro_user'] do
  password bifrost_attrs['sql_ro_password']
  superuser false
end

private_chef_pg_database 'bifrost' do
  owner bifrost_attrs['sql_user']
end

private_chef_pg_sqitch "/opt/opscode/embedded/service/oc_bifrost/db" do
  hostname postgres_attrs['vip']
  port     postgres_attrs['port']
  username bifrost_attrs['sql_user']
  password bifrost_attrs['sql_password']
  database "bifrost"
end

private_chef_pg_user_table_access bifrost_attrs['sql_user'] do
  database 'bifrost'
  schema 'public'
  access_profile :write
end

private_chef_pg_user_table_access bifrost_attrs['sql_ro_user'] do
  database 'bifrost'
  schema 'public'
  access_profile :read
end

