Rails.application.config.middleware.use OmniAuth::Builder do
  provider :chef
  provider :developer unless Rails.env.production?
end