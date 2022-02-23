# configure knife under 'root' to access the default test org as the default admin user

# Note that under older versions of chef client (<14) the config.rb file we
# are creating is not respected; instead knife expects to see a knife.rb
#
# That means if you're seeing a failure in this recipe, you're installing an old version of Chef Server.
#
# Since using the old version is probably intentional on your part, you can fix the error by changing
# the filename below to knife.rb instead of config.rb
template "/root/.chef/config.rb" do
  source "config.rb.erb"
  action :create
  owner "root"
  user "root"
  mode 0660
end

execute "knife ssl fetch" do
  cwd "/root"
  action :run
end

# Sanity check that will fail the run if something goes wrong.
# Address the issue reported, and run `vagrant provision` to retry and complete provisioning
execute "knife ssl check" do
  cwd "/root"
  action :run
end

# dev:user-env has already created /vagrant/.chef for us
# so we just need to render the correct template and copy over the keys.
#
template "/vagrant/.chef/config.rb" do
  source "config-vagrant.rb.erb"
  action :create
  owner "vagrant"
  user "vagrant"
  mode 0660
end
#
# We'll also set up this same content in /vagrant/.chef
directory "/vagrant/.chef/trusted_certs" do
  action :create
  recursive true
  owner "vagrant"
  user "vagrant"
end
# which will allow knife usage from the host. Unfortunately there is
# no resource to copy a directory, so we're stuck with execute:
#
remote_file "copy clownville-validator pem" do
  path "/vagrant/.chef/clownville-validator.pem"
  source "file:///root/.chef/clownville-validator.pem"
  owner "vagrant"
  group "vagrant"
  mode 0755
end

remote_file "copy bobo's pem" do
  path "/vagrant/.chef/bobo.pem"
  source "file:///root/.chef/bobo.pem"
  owner "vagrant"
  group "vagrant"
  mode 0755
end

remote_file "copy trusted certs for clownville" do
  path "/vagrant/.chef/trusted_certs/api_chef-server_dev.crt"
  source "file:///root/.chef/trusted_certs/api_chef-server_dev.crt"
  owner "vagrant"
  group "vagrant"
end

