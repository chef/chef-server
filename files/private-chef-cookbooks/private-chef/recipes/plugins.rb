plugins = EnterprisePluginCollection.from_glob("/opt/*/chef-server-plugin.rb")
plugins.each do |p|
  next if !p.parent_plugin.nil?
  p.enabled true if node['private_chef']['enabled-plugins']
  p.enabled false if node['private_chef']['disabled-plugins']
end

plugins.each do |plugin|
  next if !plugin.parent_plugin.nil?
  chef_run plugin.run_list do
    cookbook_path plugin.cookbook_path
    included_attrs ["private_chef"]
  end
end
