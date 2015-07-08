# NOTE:
#
# Uses the value of node['private_chef']['postgresql']['username'] as
# the user to run the database-creation psql command

def whyrun_supported?
  true
end

use_inline_resources

action :create do
  EcPostgres.with_connection(node) do |connection|
    result = connection.exec("SELECT datname FROM pg_database WHERE datname='#{new_resource.database}'")
    if result.ntuples == 0
      converge_by("Create database #{new_resource.database}") do
          owner = "WITH OWNER #{new_resource.owner}" if new_resource.owner
          connection.exec("CREATE DATABASE \"#{new_resource.database}\" #{owner} TEMPLATE #{new_resource.template} ENCODING '#{new_resource.encoding}';")
      end
    end
  end
end
