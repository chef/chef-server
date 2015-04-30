plugins = EnterprisePluginCollection.from_glob("/var/opt/opscode/plugins/*.rb")
plugins.each do |p|
  next if !p.parent_plugin.nil?
  p.enabled true if node['private_chef']['enabled-plugins'].include?(p.name)
  p.enabled false if node['private_chef']['disabled-plugins'].include?(p.name)
end

missing_plugins = node['private_chef']['enabled-plugins'] - (plugins.map {|p| p.name })
if !missing_plugins.empty?
  raise "could not find plugins: #{missing_plugins}"
end

plugins.each do |plugin|
  next if !plugin.parent_plugin.nil?
  chef_run plugin.run_list do
    cookbook_path plugin.cookbook_path
    included_attrs ["private_chef"]
  end
end
