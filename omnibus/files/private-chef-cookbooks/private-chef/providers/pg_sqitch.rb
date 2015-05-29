
def whyrun_supported?
  false
end

use_inline_resources

action :deploy do
  verify = new_resource.verify ? "--verify" : ""
  target = new_resource.target_version ? "--to-target #{new_resource.target_version}" : ""
  hostname = new_resource.hostname ? new_resource.hostname : node['private_chef']['postgresql']['vip']
  port = new_resource.port ? new_resource.port: node['private_chef']['postgresql']['port']
  execute "sqitch_deploy_#{new_resource.name}" do
    command <<-EOM.gsub(/\s+/," ").strip!
      sqitch --engine pg
             --db-host #{hostname}
             --db-port #{port}
             --db-user #{new_resource.username}
             --db-name #{new_resource.database}
             --top-dir #{new_resource.name}
             deploy #{target} #{verify}
    EOM
    environment "PERL5LIB" => "", # force sqitch to use omnibus perl only
                "SQITCH_PASSWORD" => new_resource.password
    returns [0,1]
  end
end
