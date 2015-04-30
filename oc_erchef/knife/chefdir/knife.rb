current_dir = File.dirname(__FILE__)
log_level                :info
log_location             STDOUT
# node_name                "platform-superuser"
# client_key               "#{current_dir}/superuser.pem"
node_name                "clownco-org-admin"
client_key               "#{current_dir}/clownco-org-admin.pem"
validation_client_name   "clownco-org-validator"
validation_key           "#{current_dir}/clownco-org-validation.pem"
chef_server_url          "http://localhost:4001/organizations/clownco"
cache_type               'BasicFile'
cache_options( :path => "#{ENV['HOME']}/.chef/checksums" )
cookbook_path            ["#{current_dir}/../cookbooks"]
