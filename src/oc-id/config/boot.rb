# Set up gems listed in the Gemfile.
ENV['BUNDLE_GEMFILE'] ||= File.expand_path('../../Gemfile', __FILE__)

require 'bundler/setup' if File.exist?(ENV['BUNDLE_GEMFILE'])
require "logger" # Fix concurrent-ruby removing logger dependency which Rails itself does not have

# Load CHEF_SECRETS environment data early in the boot process
# This ensures secrets are available for Rails commands and build processes
if ENV['CHEF_SECRETS_FD'].nil? && ENV['CHEF_SECRETS_DATA'].nil?
  require 'chef-utils/dist'
  secrets_file = File.expand_path("../../config/private-#{ChefUtils::Dist::Infra::SHORT}-secrets.json", __FILE__)
  if File.exist?(secrets_file)
    ENV['CHEF_SECRETS_DATA'] = File.read(secrets_file)
  end
end

