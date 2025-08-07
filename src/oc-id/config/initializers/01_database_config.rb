# Override database password configuration for Rails 7.1 compatibility
# This initializer ensures that the database password is properly set
# before ActiveRecord tries to establish a connection

# Set SQL_PASSWORD early, before Rails initializes
if ENV["CHEF_SECRETS_DATA"] && ENV["SQL_PASSWORD"].nil?
  begin
    # Parse CHEF_SECRETS_DATA JSON and extract the SQL password
    chef_secrets = JSON.parse(ENV["CHEF_SECRETS_DATA"])
    if chef_secrets['oc_id'] && chef_secrets['oc_id']['sql_password']
      ENV["SQL_PASSWORD"] = chef_secrets['oc_id']['sql_password']
      puts "SQL_PASSWORD retrieved from CHEF_SECRETS_DATA"
    end
  rescue => e
    puts "Failed to parse CHEF_SECRETS_DATA: #{e.message}"
  end
end

# Also try to get it from veil if available
if ENV["SQL_PASSWORD"].nil?
  begin
    require 'veil'
    if ENV['CHEF_SECRETS_FD'] || ENV['CHEF_SECRETS_DATA']
      provider = ENV['CHEF_SECRETS_DATA'] ? 'chef-secrets-env' : 'chef-secrets-fd'
      veil = Veil::CredentialCollection.from_config(provider: provider)
      ENV["SQL_PASSWORD"] = veil.get('oc_id', 'sql_password')
      puts "SQL_PASSWORD retrieved from veil"
    end
  rescue => e
    puts "Failed to retrieve SQL_PASSWORD from veil: #{e.message}"
  end
end

# Fallback: check if we're in a development/test environment
if ENV["SQL_PASSWORD"].nil? && (ENV["RAILS_ENV"] == "development" || ENV["RAILS_ENV"] == "test")
  ENV["SQL_PASSWORD"] = ""
  puts "Using empty password for development/test environment"
end

Rails.application.config.before_initialize do
  # Ensure database configuration is properly loaded with password
  if ENV["RAILS_ENV"] == "production" && ENV["SQL_PASSWORD"].nil?
    puts "WARNING: SQL_PASSWORD not set for production environment"
  end
end