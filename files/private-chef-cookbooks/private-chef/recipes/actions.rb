if is_data_master?

  directory "/etc/opscode-analytics" do
    owner node['private_chef']['user']['username']
    mode '0775'
    recursive true
  end

  # Write out the config files for actions to load in order to interface with this EC
  # instance
  #
  file "/etc/opscode-analytics/webui_priv.pem" do
    owner node["private_chef"]["user"]["username"]
    group "root"
    mode "0600"
    content lazy {::File.open('/etc/opscode/webui_priv.pem').read}
  end

  file "/etc/opscode-analytics/actions-source.json" do
    owner 'root'
    mode '0600'
    content Chef::JSONCompat.to_json_pretty({
      private_chef: {
        api_fqdn:           node['private_chef']['lb']['api_fqdn'],
        rabbitmq_host:      node['private_chef']['rabbitmq']['vip'],
        rabbitmq_port:      node['private_chef']['rabbitmq']['node_port'],
        rabbitmq_vhost:     node['private_chef']['rabbitmq']['actions_vhost'],
        rabbitmq_exchange:  node['private_chef']['rabbitmq']['actions_exchange'],
        rabbitmq_user:      node['private_chef']['rabbitmq']['actions_user'],
        rabbitmq_password:  node['private_chef']['rabbitmq']['actions_password']
      }
    })
  end


end
