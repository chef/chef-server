Rails.application.config.middleware.use OmniAuth::Builder do
  provider :chef, endpoint: Settings.chef.endpoint, superuser: Settings.chef.superuser, key_path: Settings.chef.key_path
  provider :developer unless Rails.env.production?
end

OmniAuth.config.on_failure = Proc.new { |env|
  OmniAuth::FailureEndpoint.new(env).redirect_to_failure
}

# Use Rails's logger
OmniAuth.config.logger = Rails.logger
