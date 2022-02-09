## Set up a test org, a user, and assign the user as an admin user

# Unfortunately chef client does not come with resources to do this for us
directory "/root/.chef" do
  action :create
  mode 0770
end

execute "create user janedoe" do
  command "chef-server-ctl user-create bobo bobo theclown bobo@example.com 'password' --filename /root/.chef/bobo.pem"
  creates "/root/.chef/bobo.pem"
  action :run
end

execute "create org clownville" do
  command "chef-server-ctl org-create clownville 'Clownville' --filename /root/.chef/clownville-validator.pem"
  creates "/root/.chef/clownville-validator.pem"
  action :run
end

execute "associate bobo with clownville as an admin user" do
  command "chef-server-ctl org-user-add clownville bobo -a"
  action :run
end
