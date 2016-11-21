## System setup
# Hey neat - our packages have chef from master now
# which means we can:
package node['install_packages']

# Time and zone should match the host so that erlang's sync module plays nicely with rsync'd files.
file "/etc/timezone" do
  content node["tz"]
  owner 'root'
  group 'root'
  mode 0644
  notifies :run, 'bash[dpkg-reconfigure tzdata]'
end

bash 'dpkg-reconfigure tzdata' do
  user 'root'
  code "/usr/sbin/dpkg-reconfigure -f noninteractive tzdata"
  action :nothing
end


template "/etc/profile.d/omnibus-embedded.sh" do
  source "omnibus-embedded.sh.erb"
  owner "root"
  user "root"
  mode 0644
end

template "/etc/sudoers" do
  source "sudoers"
  action :create
  owner "root"
  user "root"
  mode 0440
end

file "/etc/update-motd.d/10-help-text" do
  action :delete
end
file "/etc/update-motd.d/91-release-upgrade" do
  action :delete
end

template "/etc/motd" do
  source "motd.erb"
  owner "root"
  user "root"
  mode 0644
end

# It seems that we're getting an ssh configured to look for
execute "generate ed25519 ssh host key" do
  command "dpkg-reconfigure openssh-server"
  not_if { File.exist?("/etc/ssh/ssh_host_ed25519_key") }
end
