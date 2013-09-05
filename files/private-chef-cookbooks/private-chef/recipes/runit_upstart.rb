#
# Copyright:: Copyright (c) 2013 Opscode, Inc.
#
# All Rights Reserved
#

# Ensure the previous named iteration of the system job is nuked
execute "initctl stop opscode-runsvdir" do
  only_if "initctl status opscode-runsvdir | grep start"
  retries 30
end

file "/etc/init/opscode-runsvdir.conf" do
  action :delete
end

template "/etc/init/private-chef-runsvdir.conf" do
  owner "root"
  group "root"
  mode "0644"
  variables({
      :install_path => "/opt/opscode"
  })
  source "init-runsvdir.erb"
end

# Keep on trying till the job is found :(
execute "initctl status private-chef-runsvdir" do
  retries 30
end

# If we are stop/waiting, start
#
# Why, upstart, aren't you idempotent? :(
execute "initctl start private-chef-runsvdir" do
  only_if "initctl status private-chef-runsvdir | grep stop"
  retries 30
end
