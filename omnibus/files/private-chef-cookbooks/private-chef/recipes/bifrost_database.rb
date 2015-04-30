# This recipe assumes that the database server has already been set up
# and is running.

# Extract the attribute hash here so we're not quite so verbose
bifrost_attrs = node['private_chef']['oc_bifrost']

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

execute "bifrost_schema" do
  # The version of the schema to be deployed will the the maximum
  # available in the oc_bifrost repository.  This will be the same
  # version needed by the code that is deployed here.  If we ever
  # split bifrost's code and schema into separate repositories,
  # we'll need to deploy to a specific schema tag
  command <<-EOM.gsub(/\s+/," ").strip!
    sqitch --engine pg
           --db-name bifrost
           --top-dir /opt/opscode/embedded/service/oc_bifrost/db
           deploy --verify
  EOM
  user node['private_chef']['postgresql']['username']
  # Clear PERL5LIB to ensure sqitch only uses omnibus's perl
  # installation
  environment "PERL5LIB" => ""
  # If sqitch is deploying the first time, it'll return 0 on
  # success.  If it's running a second time and ends up deploying
  # nothing (since we've already deployed all changesets), it'll
  # return 1.  Both scenarios should be considered successful.
  returns [0,1]
end

# Permissions for the database users got set in the schema... though
# that means that the role names should be hard-coded in this
# cookbook.
execute "add_permissions_bifrost" do
  command <<-EOM.gsub(/\s+/," ").strip!
      psql --dbname bifrost
           --single-transaction
           --set ON_ERROR_STOP=1
           --set database_name=bifrost
           --file sql/permissions.sql
    EOM
  cwd "/opt/opscode/embedded/service/oc_bifrost/db"
  user node['private_chef']['postgresql']['username']
  # This can run each time, since the commands in the SQL file are all
  # idempotent anyway
end
