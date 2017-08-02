def fetch_chef_server_version
  attribute('application_version', default: ENV['CHEF_SERVER_VERSION'])
end

def fetch_target_host
  attribute('target_host', default: command('cat /etc/opscode/chef-server.rb | grep -Po "api_fqdn\s[\"\']+\K.*(?=[\"\']+?)"').stdout.strip)
end
