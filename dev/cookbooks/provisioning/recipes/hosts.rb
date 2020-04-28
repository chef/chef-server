template "/etc/hosts" do
  source "hosts.erb"
  owner "root"
  group "root"
  action :create
  mode  "0644"
  variables({"fqdns" => ["api.#{Chef::Dist::Server::SHORT}.dev",  "manage.#{Chef::Dist::Server::SHORT}.dev"],
             "global_fqdns" => node['provisioning']['hosts']})

end
