def fetch_chef_server_version
  attribute('application_version', default: ENV['APPLICATION_VERSION'])
end

def fetch_target_host
  inspec.backend.hostname
end
