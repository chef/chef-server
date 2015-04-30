require File.expand_path('../boot', __FILE__)

# Pick the frameworks you want:
require "active_record/railtie"
require "action_controller/railtie"
require "action_mailer/railtie"
require "sprockets/railtie"
# require "rails/test_unit/railtie"

# Require the gems listed in Gemfile, including any gems
# you've limited to :test, :development, or :production.
Bundler.require(:default, Rails.env)

module OcId
  class Application < Rails::Application
    # Settings in config/environments/* take precedence over those specified here.
    # Application configuration should go into files in config/initializers
    # -- all .rb files in that directory are automatically loaded.

    # Set Time.zone default to the specified zone and make Active Record auto-convert to this zone.
    # Run "rake -D time" for a list of tasks for finding time zone names. Default is UTC.
    # config.time_zone = 'Central Time (US & Canada)'

    # The default locale is :en and all translations from config/locales/*.rb,yml are auto loaded.
    # config.i18n.load_path += Dir[Rails.root.join('my', 'locales', '*.{rb,yml}').to_s]
    # config.i18n.default_locale = :de

    I18n.enforce_available_locales = true

    config.to_prepare do
      Doorkeeper::ApplicationController.layout 'application'
      Doorkeeper::ApplicationsController.layout 'application'
      Doorkeeper::AuthorizationsController.layout 'application'
      Doorkeeper::AuthorizedApplicationsController.layout 'application'
    end

    config.autoload_paths += %W(#{config.root}/lib)
    config.assets.prefix = "/id/assets"

    # If you want to use the rails_config Settings object here in application.rb, uncomment below.
    # config.before_initialize do
    #   puts Settings.yay.me
    # end

    config.action_mailer.default_url_options = {
      host: Settings.origin,
      port: Settings.port,
      protocol: Settings.protocol
    }
    config.action_mailer.delivery_method = :sendmail
  end
end
