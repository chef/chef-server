template "/etc/hosts" do
  source "hosts.erb"
  owner "root"
  group "root"
  action :create
  variables({"fqdns" => {"api.chef-server.dev" => "192.168.33.100",  "node.chef-server.dev" => "192.168.33.101"}})
end

directory "/etc/opscode" do
  owner "root"
  group "root"
  recursive true
  action :create
end

# Bare minimum packages for other stuff to work:
execute "apt-get-update" do
  command "apt-get update"
  ignore_failure true
  not_if do
    File.exists?('/var/chef/cache/apt-update-done')
  end
end

file "/var/chef/cache/apt-update-done" do
  action :create
end
package "build-essential"
package "git"

