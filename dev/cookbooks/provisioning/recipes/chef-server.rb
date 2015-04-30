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

# Install required external packages.
# TODO eventually support auto-download of these packages from packagecloud
 #node['chef-server']['installers'].each do |package_name|
  #package package_name do
    #source "/installers/#{package_name}"
    #provider Chef::Provider::Package::Dpkg
    #action :install
    #not_if { File.exists? "/var/chef/cache/#{package_name}-installed" }
  #end
  #file "/var/chef/cache/#{package_name}-installed" do
    #action :create
  #end

#end

directory "/etc/opscode" do
  owner "root"
  group "root"
  recursive true
  action :create
end

template "/etc/opscode/chef-server.rb" do
  source "chef-server.rb.erb"
  owner "root"
  group "root"
  action :create
  notifies :run, "execute[reconfigure]", :immediately
end

template "/etc/hosts" do
  source "hosts.erb"
  owner "root"
  group "root"
  action :create
  variables({"fqdns" => ["api.chef-server.dev",  "manage.chef-server.dev" ]})
end

execute "reconfigure" do
  command "chef-server-ctl reconfigure"
  action :nothing
end

