
action :create do

  keepalived_dir = node['private_chef']['keepalived']['dir']
  keepalived_etc_dir = File.join(keepalived_dir, "etc")
  keepalived_log_dir = node['private_chef']['keepalived']['log_directory']

  directory keepalived_log_dir do
    owner node['private_chef']['user']['username']
    recursive true
    mode "0700"
  end

  # rewrite the sv/keepalived/run file to have the correct flags before a restart
  component_runit_service "keepalived"

  # override default keepalived state to MASTER so we don't stop
  keepalived_options = node['private_chef']['keepalived'].to_hash
  keepalived_options['vrrp_instance_state'] = 'MASTER'

  template File.join(keepalived_etc_dir, "keepalived.conf") do
    source "keepalived.conf.erb"
    owner "root"
    group "root"
    mode "0644"
    variables(keepalived_options)
    notifies :restart, 'runit_service[keepalived]', :immediately
  end

end