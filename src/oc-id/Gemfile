source 'https://rubygems.org'

gem 'rails', '= 4.2.8'
gem 'chef', '~> 12'
gem 'jbuilder', '~> 1.2'
gem 'jquery-rails'
gem 'jwt' # For Zendesk SSO
gem 'rails_config', '~> 0.4.2'
gem 'rb-readline', '~> 0.5.2', require: false
gem 'sass-rails', '>= 4.0.3'
gem 'turbolinks', '~> 2.2.1'
gem 'unicorn-rails', '~> 1.1.0'
gem 'omniauth', '~> 1.3.2'
gem 'nokogiri', '= 1.8.1' # PIN because 1.8.4 doesn't seem to be building in omnibus...
gem 'pg', '~> 0.15' # active_record 4.2.8 pins this but doesn't manifest this in the gemspec for some reason
gem 'mixlib-authentication', '~> 1.3'
gem 'sentry-raven'
gem 'responders', '~> 2.0'
gem 'newrelic_rpm'
gem 'doorkeeper', '~> 4.0'

gem 'veil', git: 'https://github.com/chef/chef_secrets'
gem 'omniauth-chef', git: 'https://github.com/chef/omniauth-chef'
gem 'chef-web-core', git: 'https://github.com/chef/chef-web-core.git'

#
# These gems require a javascript runtime.  We don't want to ship a
# javascript runtime so we put them in a separate group such that they
# don't get automatically required by rake.
#
group :node do
  gem 'coffee-rails', '~> 4.0.0'
  gem 'uglifier', '~> 2.4.0'
end

group :development, :test do
  # Loading it this way makes it work with Ruby 2.1.2. See
  # https://github.com/nixme/jazz_hands/issues/25
  gem 'jazz_hands', git: 'https://github.com/chef/jazz_hands', branch: 'praj/bring-your-own-debugger'
  gem 'rspec-rails', '~> 3.2'
  gem 'pry-byebug'
  gem 'mailcatcher'
end

group :development do
  gem 'better_errors'
  gem 'binding_of_caller'
  gem 'quiet_assets'
  gem 'spring' # App preloading
  gem 'spring-commands-rspec'
  gem 'thor', '~> 0.18.0'
end

group :doc do
  gem 'sdoc', require: false
end

group :test do
  gem 'capybara', '~> 2.4.4'
  gem 'factory_girl_rails', '~> 4.4.0'
  gem 'selenium-webdriver', '~> 2.53.0'
  gem 'timecop'
end
