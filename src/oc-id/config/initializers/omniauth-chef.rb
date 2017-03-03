

Rails.application.config.middleware.use OmniAuth::Builder do
  configure do |config|
    config.path_prefix = '/id/auth'
  end

  file = Settings.chef.secrets_file || '/etc/opscode/private-chef-secrets.json'
  veil = Veil::CredentialCollection::ChefSecretsFile.from_file(file)
  provider :chef, Settings.chef.to_hash.merge(key_data: veil.get("chef-server", "webui_key"))
end

OmniAuth.config.on_failure = proc do |env|
  OmniAuth::FailureEndpoint.new(env).redirect_to_failure
end

# Use Rails's logger
OmniAuth.config.logger = Rails.logger
