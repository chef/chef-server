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
