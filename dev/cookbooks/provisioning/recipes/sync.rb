file "/vagrant/dvm-file-sync-stats.json" do
  action :delete
end

execute "/vagrant/scripts/dvm-fs-sync start"
