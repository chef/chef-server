
def whyrun_supported?
  true
end

use_inline_resources

action :deploy do
  target = new_resource.target_version ? "--to-target #{new_resource.target_version}" : ""
  converge_by "Deploying schema from #{new_resource.name}" do
    execute "sqitch_deploy_#{new_resource.name}" do
      command <<-EOM.gsub(/\s+/," ").strip!
        sqitch --engine pg
               --db-name #{new_resource.database}
               --db-host #{new_resource.hostname}
               --db-port #{new_resource.port}
               --db-user #{new_resource.username}
               --top-dir #{new_resource.name}
               deploy #{target} --verify
      EOM
      environment "PERL5LIB" => "", # force us to use omnibus perl
                  "PGPASSWORD" => new_resource.password

      # Sqitch Return Codes
      # 0 - when changes are applied
      # 1 - when everything is ok but no changes were made
      # 2(+?) - when an error occurs.
      returns [0,1]
    end
  end
end
