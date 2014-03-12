# coding: utf-8

lib = File.expand_path '../lib', __FILE__

$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include? lib

require 'omniauth-chef/version'

Gem::Specification.new do |spec|
  spec.name        = 'omniauth-chef'
  spec.version     = OmniAuth::Chef::VERSION
  spec.authors     = ['Allen Goodman']
  spec.email       = %q(a@getchef.com)
  spec.description = %q{OmniAuth strategy for Chef}
  spec.summary     = %q{OmniAuth strategy for Chef}
  spec.homepage    = 'https://github.com/opscode/oc_actionlog'

  spec.files  = %w(.gitignore omniauth-chef.gemspec Gemfile Rakefile)
  spec.files += Dir.glob 'lib/**/*.rb'
  spec.files += Dir.glob 'bin/**/*'

  spec.test_files = Dir.glob 'spec/**/*.rb'

  spec.require_paths = Dir.glob 'lib/**/*.rb'

  spec.add_development_dependency 'bundler'
  spec.add_development_dependency 'rack-test'
  spec.add_development_dependency 'rake'
  spec.add_development_dependency 'rspec'
  spec.add_development_dependency 'webmock'

  spec.add_runtime_dependency 'chef', '~> 11.10.4'
  spec.add_runtime_dependency 'omniauth-oauth'
end
