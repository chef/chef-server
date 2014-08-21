provider = node['private_chef']['ha']['provider']
plugins = EnterprisePluginCollection.from_glob("/opt/*/chef-server-plugin.rb")
ha_plugin = plugins.find { |p| p.name == "chef-ha-#{provider}" }

if ha_plugin.nil?
  raise "Couldn't find plugin for HA provider: #{provider}"
else
  ha_plugin.enabled true
end

chef_run ha_plugin.run_list do
  cookbook_path ha_plugin.cookbook_path
  included_attrs ["private_chef"]
end

