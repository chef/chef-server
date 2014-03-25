source 'https://rubygems.org'

gem 'rails', '4.0.3'
gem 'sass-rails', '~> 4.0.0'
gem 'coffee-rails', '~> 4.0.0'
gem 'jquery-rails', '~> 3.1.0'
gem 'rails_config', '~> 0.3.3'
gem 'turbolinks', '~> 2.2.1'
gem 'jbuilder', '~> 1.2'
gem 'uglifier', '~> 2.4.0'
gem 'bootstrap-sass', '~> 3.1.1'
gem 'chef', '~> 11.10.4'
gem 'rb-readline', '~> 0.4.2', require: false
gem 'omniauth-chef', path: 'lib/omniauth-chef'

# Don't judge me. I'm just waiting for a release that includes a recent Bootstrap 3 merge.
gem 'doorkeeper', github: 'applicake/doorkeeper', ref: 'master'

# Temporary, of course; will be PG
gem 'sqlite3', '~> 1.3.9'

group :development do
  gem 'rspec-rails', '2.13.1'
  gem 'jazz_hands', '~> 0.5.1'
  gem 'thor', '~> 0.18.0'
end

group :test do
  gem 'factory_girl_rails', '~> 4.4.0'
  gem 'selenium-webdriver', '2.35.1'
  gem 'capybara', '2.1.0'
end

group :production do
  gem 'unicorn', '~> 4.3.1', require: false
end

group :doc do
  gem 'sdoc', require: false
end
