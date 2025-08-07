# Early database configuration for Rails 7.1 compatibility
# This must run before any other initializers that might try to connect to the database

# Ensure SQL_PASSWORD is available very early in the boot process
if ENV["RAILS_ENV"] == "production"
  # Try multiple methods to get the SQL password
  sql_password = nil
  
  # Method 1: Check if already set
  sql_password = ENV["SQL_PASSWORD"] if ENV["SQL_PASSWORD"]
  
  # Method 2: Try to get from CHEF_SECRETS_DATA
  if sql_password.nil? && ENV["CHEF_SECRETS_DATA"]
    begin
      chef_secrets = JSON.parse(ENV["CHEF_SECRETS_DATA"])
      if chef_secrets.dig('oc_id', 'sql_password')
        sql_password = chef_secrets['oc_id']['sql_password']
        ENV["SQL_PASSWORD"] = sql_password
        puts "SQL_PASSWORD loaded from CHEF_SECRETS_DATA"
      end
    rescue => e
      puts "Warning: Failed to parse CHEF_SECRETS_DATA: #{e.message}"
    end
  end
  
  # Method 3: Try to get from veil if available
  if sql_password.nil?
    begin
      require 'veil'
      if ENV['CHEF_SECRETS_FD'] || ENV['CHEF_SECRETS_DATA']
        provider = ENV['CHEF_SECRETS_DATA'] ? 'chef-secrets-env' : 'chef-secrets-fd'
        veil = Veil::CredentialCollection.from_config(provider: provider)
        sql_password = veil.get('oc_id', 'sql_password')
        if sql_password
          ENV["SQL_PASSWORD"] = sql_password
          puts "SQL_PASSWORD loaded from veil"
        end
      end
    rescue => e
      puts "Warning: Failed to load SQL_PASSWORD from veil: #{e.message}"
    end
  end
  
  # Method 4: Check environment variables that might be set by veil-env-helper
  if sql_password.nil?
    # veil-env-helper might set these environment variables
    %w[OC_ID_SQL_PASSWORD POSTGRESQL_PASSWORD DB_PASSWORD].each do |env_var|
      if ENV[env_var]
        sql_password = ENV[env_var]
        ENV["SQL_PASSWORD"] = sql_password
        puts "SQL_PASSWORD loaded from #{env_var}"
        break
      end
    end
  end
  
  # If we still don't have a password, warn but don't fail
  if sql_password.nil?
    puts "WARNING: SQL_PASSWORD not found. Database connection may fail."
    puts "Available environment variables: #{ENV.keys.select { |k| k.include?('SQL') || k.include?('PASSWORD') || k.include?('DB') }.join(', ')}"
  end
end
