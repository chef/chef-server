file "/vagrant/dvm-file-sync-stats.json" do
  action :delete
end
cookbook_file "/etc/init/dvm-fs-sync.conf" do
  source "dvm-fs-sync.conf"
  mode "0644"
  owner "root"
  group "root"
end

cookbook_file "/etc/init.d/dvm-fs-sync" do
  source "dvm-fs-sync"

  mode "0755"
  owner "root"
  group "root"
end

service "dvm-fs-sync" do
  supports start: true, stop: true, status: true
  start_command "/etc/init.d/dvm-fs-sync start"
  action [ :enable, :start ]
end
