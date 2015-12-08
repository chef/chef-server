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

template "/etc/hosts" do
  source "analytics_hosts.erb"
  owner "root"
  group "root"
  action :create
  variables({"fqdns" => ["chef-analytics.dev"]})
end

directory "/etc/opscode-analytics" do
  owner "root"
  group "root"
  recursive true
  action :create
end

# Note that we do not run reconfigure at this time
# We will allow the dvm recipe to handle when that should occur.
template "/etc/opscode-analytics/opscode-analytics.rb" do
  source "opscode-analytics.rb.erb"
  owner "root"
  group "root"
  action :create
end


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


