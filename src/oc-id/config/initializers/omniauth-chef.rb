

Rails.application.config.middleware.use OmniAuth::Builder do
  configure do |config|
    config.path_prefix = '/id/auth'
  end

  provider :chef, Settings.chef.to_hash.merge(key_data: Secrets.get("chef-server", "webui_key"))
end

OmniAuth.config.on_failure = proc do |env|
  OmniAuth::FailureEndpoint.new(env).redirect_to_failure
end

# Use Rails's logger
OmniAuth.config.logger = Rails.logger
