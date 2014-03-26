source 'https://rubygems.org'

gem 'rails', '4.0.3'

gem 'bootstrap-sass', '~> 3.1.1'
gem 'chef', '~> 11.10.4'
gem 'coffee-rails', '~> 4.0.0'
gem 'jbuilder', '~> 1.2'
gem 'jquery-rails', '~> 3.1.0'
gem 'omniauth-chef', path: 'lib/omniauth-chef'
gem 'rails_config', '~> 0.3.3'
gem 'rb-readline', '~> 0.4.2', require: false
gem 'sass-rails', '~> 4.0.0'
gem 'turbolinks', '~> 2.2.1'
gem 'uglifier', '~> 2.4.0'
gem 'unicorn-rails'

# Don't judge me. I'm just waiting for a release that includes a recent Bootstrap 3 merge.
gem 'doorkeeper', github: 'applicake/doorkeeper', ref: 'master'

# Temporary, of course; will be PG
gem 'sqlite3', '~> 1.3.9'

group :development, :test do
  gem 'jazz_hands', '~> 0.5.1'
  gem 'rspec-rails'
end

group :development do
  gem 'quiet_assets'
  gem 'spring' # App preloading
  gem 'spring-commands-rspec'
  gem 'thor', '~> 0.18.0'
end

group :doc do
  gem 'sdoc', require: false
end
group :test do
  gem 'capybara', '2.1.0'
  gem 'factory_girl_rails', '~> 4.4.0'
  gem 'selenium-webdriver', '2.35.1'
end


