
def whyrun_supported?
  true
end

use_inline_resources

action :deploy do
  target = new_resource.target_version ? "--to-target #{new_resource.target_version}" : ""

  # Note: the split behavior below will go away once we modify managed local psql
  # to use permit password + tcp for postgres user on localhost.
  if new_resource.password.empty?
    auth_info = ""
    run_user = new_resource.username
  else
    run_user = Process.uid
    auth_info = "--db-host #{new_resource.hostname} --db-port #{new_resource.port} --db-user #{new_resource.username}"
  end

  converge_by "Deploying schema from #{new_resource.name}" do
    execute "sqitch_deploy_#{new_resource.name}" do
      command <<-EOM.gsub(/\s+/," ").strip!
        sqitch --engine pg #{auth_info}
               --db-name #{new_resource.database}
               --top-dir #{new_resource.name}
               deploy #{target} --verify
      EOM
      environment "PERL5LIB" => "", # force us to use omnibus perl
                  "PGPASSWORD" => new_resource.password
      user run_user

      # Sqitch Return Codes
      # 0 - when changes are applied
      # 1 - when everything is ok but no changes were made
      # 2(+?) - when an error occurs.
      returns [0,1]
    end
  end
end
