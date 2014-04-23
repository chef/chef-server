source 'https://rubygems.org'

gem 'rails', '4.0.5'
gem 'bootstrap-sass', '~> 3.1.1'
gem 'chef', '~> 11.10.4'
gem 'coffee-rails', '~> 4.0.0'
gem 'jbuilder', '~> 1.2'
gem 'jquery-rails', '~> 3.1.0'
gem 'rails_config', '~> 0.3.3'
gem 'rb-readline', '~> 0.4.2', require: false
gem 'sass-rails', '~> 4.0.0'
gem 'turbolinks', '~> 2.2.1'
gem 'uglifier', '~> 2.4.0'
gem 'unicorn-rails', '~> 1.1.0'
gem 'omniauth', '~> 1.2.1'
gem 'omniauth-chef', github: 'opscode/omniauth-chef', ref: '0388f58f2ce8b4560ccacba3faee24b4928e7880'
gem 'nokogiri', '~> 1.6.2'

# Still waiting for a release that includes a recent Bootstrap 3 merge.
gem 'doorkeeper', github: 'applicake/doorkeeper', ref: 'cc9fa8a1b1b74ab50d542db186e9501e92d479b4'

group :production do
  gem 'pg', '~> 0.17.1'
end

group :development, :test do
  gem 'sqlite3', '~> 1.3.9'
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
