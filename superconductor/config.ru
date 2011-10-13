require 'rubygems'
require 'bundler/setup'

Bundler.require

$:.unshift File.expand_path('../app', __FILE__)
require 'superconductor'

environment = case ENV['RACK_ENV']
when /^dev/, nil
  "development"
when /prod/
  "production"
else
  abort "Unknown environment #{ENV['RACK_ENV']}, giving up"
end

puts "Running superconductor in environment #{environment}"

require File.expand_path("../config/#{environment}", __FILE__)

run Superconductor
