if is_data_master?
  # Contents of the OC ID app's JSON data, to be called later
  oc_id_app = proc do
    begin
      Chef::JSONCompat.from_json(
        open('/etc/opscode/oc-id-applications/analytics.json').read
      )
    rescue Errno::ENOENT
      Chef::Log.warn('No analytics oc-id-application present. Skipping')
      {}
    end
  end

  directory "/etc/opscode-analytics" do
    owner OmnibusHelper.new(node).ownership['owner']
    group OmnibusHelper.new(node).ownership['group']
    mode '0775'
    recursive true
  end

  # Write out the config files for actions to load in order to interface with this EC
  # instance
  # TODO 2017-02-28 mp: well this won't do...
  file "/etc/opscode-analytics/webui_priv.pem" do
    owner OmnibusHelper.new(node).ownership['owner']
    group "root"
    mode "0600"
    sensitive true
    content lazy { PrivateChef.credentials.get('chef-server', 'webui_key') }
  end

  rabbitmq = OmnibusHelper.new(node).rabbitmq_configuration

  file "/etc/opscode-analytics/actions-source.json" do
    owner 'root'
    mode '0600'
    content lazy {
      Chef::JSONCompat.to_json_pretty(
        private_chef: {
          api_fqdn:           node['private_chef']['lb']['api_fqdn'],
          oc_id_application:  oc_id_app.call,
          rabbitmq_host:      rabbitmq['vip'],
          rabbitmq_port:      rabbitmq['node_port'],
          rabbitmq_vhost:     rabbitmq['actions_vhost'],
          rabbitmq_exchange:  rabbitmq['actions_exchange'],
          rabbitmq_user:      rabbitmq['actions_user'],
          rabbitmq_password:  rabbitmq['actions_password']
        }
      )
    }
  end
end
