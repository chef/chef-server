
template "/etc/opscode/private-chef.sh" do
  source "private-chef-sh.erb"
  mode "0644"
  owner "root"
  group "root"
end
