# NOTE:
#
# Uses the value of node['private_chef']['postgresql']['username'] as
# the user to run the user-creation psql command

def whyrun_supported?
  true
end

use_inline_resources

action :create do
  EcPostgres.with_connection(node, new_resource.database) do |connection|
    run_sql(connection, "GRANT CONNECT ON DATABASE #{new_resource.database} TO #{new_resource.username}")
    run_sql(connection, "ALTER DEFAULT PRIVILEGES IN SCHEMA #{new_resource.schema} GRANT #{Array(new_resource.access).join(', ')} ON TABLES TO #{new_resource.username};")
    run_sql(connection, "GRANT #{Array(new_resource.access).join(', ')} ON ALL TABLES IN SCHEMA #{new_resource.schema} TO #{new_resource.username}")
  end
end

def run_sql(connection, sql, *params)
  converge_by sql do
    connection.exec(sql, *params)
  end
end
