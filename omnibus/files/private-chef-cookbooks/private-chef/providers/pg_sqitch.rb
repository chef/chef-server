
def whyrun_supported?
  true
end

use_inline_resources

action :deploy do
  target = new_resource.target_version ? "--to-target #{new_resource.target_version}" : ""
  execute "sqitch_deploy_#{new_resource.name}" do
    command <<-EOM.gsub(/\s+/," ").strip!
      sqitch --engine pg
             --db-host #{new_resource.hostname}
             --db-port #{new_resource.port}
             --db-user #{new_resource.username}
             --db-name #{new_resource.database}
             --top-dir #{new_resource.name}
             deploy #{target} --verify
    EOM
    environment "PERL5LIB" => "",
                "PGPASSWORD" => new_resource.password

    # Sqitch Return Codes
    # 0 - when changes are applied
    # 1 - when everything is ok but no changes were made
    # 2(+?) - when an error occurs.
    returns [0,1]
  end
end
