# coding: utf-8
lib = File.expand_path("../lib", __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require "libcs/version"

Gem::Specification.new do |spec|
  spec.name          = "libcs"
  spec.version       = LibCS::VERSION
  spec.authors       = ["Chef Software, Inc"]
  spec.email         = ["maintainers@chef.io"]

  spec.summary       = %q{A library for shared ruby code in Chef Server.}
  spec.description   = %q{LibCS contains library functions used in Chef Server's reconfigure cookbooks and in the command line tool chef-server-ctl.}
  spec.homepage      = ""

  spec.files         = Dir["lib/**/*.rb"]
  spec.bindir        = "bin"
  spec.executables   = []
  spec.require_paths = ["lib"]

  # External Dependencies
  spec.add_dependency "rb-readline"
  spec.add_dependency "highline"
  spec.add_dependency "sequel"
  spec.add_dependency "redis", "3.3.3"
  spec.add_dependency "rest-client", "1.8.0"
  # TODO(ssd) 2017-07-20: Locked to the same version as the omnibus
  # software install since we still want to install this via omnibus
  # to pick up all the correct pg libs.
  spec.add_dependency "pg", "= 0.17.1"

  # Chef Software dependencies, check the Gemfile
  # since some of these point at git
  spec.add_dependency "veil"
  spec.add_dependency "knife-ec-backup"
  spec.add_dependency "chef_backup"
  spec.add_dependency "knife-opc"
  spec.add_dependency "mixlib-install", "2.1.5"

  spec.add_development_dependency "bundler", "~> 1.15"
  spec.add_development_dependency "rake", "~> 10.0"
  spec.add_development_dependency "rspec", "~> 3.0"
  spec.add_development_dependency "chefstyle"
end
