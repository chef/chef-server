def fetch_chef_server_version
  attribute('application_version', default: ENV['CHEF_SERVER_VERSION'])
end

def fetch_target_host
  attribute('target_host', default: command('grep -Po "api_fqdn\s[\"\']+\K.*(?=[\"\']+?)" /etc/opscode/chef-server.rb').stdout.strip)
end
