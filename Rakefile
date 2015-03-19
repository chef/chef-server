require 'bundler'

namespace :test do
  task :csc do
    Bundler.with_clean_env { system "cd files/private-chef-ctl-commands/ && bundle install && bundle exec rspec" }
  end
end

