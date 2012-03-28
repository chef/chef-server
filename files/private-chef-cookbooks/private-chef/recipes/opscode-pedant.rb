template "/etc/opscode/pedant_config.rb" do
  owner "root"
  group "root"
  mode  "0755"
  variables :api_url  => node['private_chef']['nginx']['url'],
            :solr_url => node['private_chef']['opscode-solr']['url']
end
