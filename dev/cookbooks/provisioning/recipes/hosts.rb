template "/etc/hosts" do
  source "hosts.erb"
  owner "root"
  group "root"
  action :create
  variables({"fqdns" => ["api.chef-server.dev",  "manage.chef-server.dev"],
             "global_fqdns" => node['provisioning']['hosts']})

end
