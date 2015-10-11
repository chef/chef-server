

# Note that we do not run reconfigure at this time
# We will allow the dvm recipe to handle when that should occur.
template "/etc/opscode/chef-server.rb" do
  source "chef-server.rb.erb"
  owner "root"
  group "root"
  action :create
  variables( fqdn: 'api.chef-server.dev',
             ip: '192.168.33.100' )
end


