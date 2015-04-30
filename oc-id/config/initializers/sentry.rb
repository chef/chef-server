Raven.configure do |config|
  if Settings.sentry_dsn.present?
    config.dsn = Settings.sentry_dsn
    config.environments = [Rails.env]
  else
    config.environments = []
  end

  config.logger = Rails.logger
  config.processors = [Raven::Processor::SanitizeData]
end
