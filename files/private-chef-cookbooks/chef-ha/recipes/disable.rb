plugins = EnterprisePluginCollection.from_glob("/opt/*/chef-server-plugin.rb")
ha_plugins = plugins.select { |p| p.parent_plugin == "chef-ha" }

ha_plugins.each do |plugin|
  plugin.enabled false
  chef_run plugin.run_list do
    cookbook_path plugin.cookbook_path
    included_attrs ["private_chef"]
  end
end
