## System setup
node['packages'].each do |p|
  package p
end

# Time and zone should match the host so that erlang's sync module plays nicely with vagrant rsync
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
  mode "0600"
end

template "/etc/sudoers" do
  source "sudoers"
  action :create
  owner "root"
  user "root"
  mode 0440
end



