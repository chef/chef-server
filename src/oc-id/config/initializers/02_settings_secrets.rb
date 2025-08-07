# Settings configuration with secrets integration for Rails 7.1
# This initializer ensures that Settings object is properly configured with secrets

Rails.application.config.after_initialize do
  # Override Settings with secrets from veil
  begin
    if defined?(Secrets)
      # Override secret_key_base with value from secrets
      secret_key_base = Secrets.get('oc_id', 'secret_key_base')
      if secret_key_base && secret_key_base != 'CHANGE ME'
        Settings.secret_key_base = secret_key_base
        puts "secret_key_base loaded from secrets"
      else
        puts "WARNING: secret_key_base not found in secrets or is default value"
      end
    end
  rescue => e
    puts "Failed to load secret_key_base from secrets: #{e.message}"
  end
end
