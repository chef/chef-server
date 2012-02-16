name "private-chef-cookbooks"

build do
  cookbook_dir = File.expand_path("../files/private-chef-cookbooks", __FILE__)
  command "mkdir -p /opt/opscode/embedded/cookbooks"
  command "cp -ra #{cookbook_dir}/* /opt/opscode/embedded/cookbooks/"
  command "ln -sf /opt/opscode/embedded/cookbooks/bin/private-chef-ctl /opt/opscode/bin/private-chef-ctl"
end
