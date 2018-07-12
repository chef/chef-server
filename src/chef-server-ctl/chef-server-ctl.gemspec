
lib = File.expand_path("../lib", __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require "chef_server_ctl/version"


Gem::Specification.new do |spec|
  spec.name          = "chef-server-ctl"
  spec.version       = ChefServerCtl::VERSION
  spec.authors       = ["Mark Anderson"]
  spec.email         = ["mark@chef.io"]
  spec.description   = %q{Commands to control Chef Server}
  spec.summary       = spec.description
  spec.licenses      = "Apache2"

  spec.files         = %w{LICENSE README.md} + Dir.glob("{bin,doc,helpers,lib,plugins,spec}/**/*")
  spec.bindir        = "bin"
  spec.executables   = "chef-server-ctl"
  spec.test_files    = spec.files.grep(%r{^(test|spec|features)/})
  spec.require_paths = ["lib"]

  spec.required_ruby_version = ">= 2.4.0"

  spec.add_runtime_dependency "highline", "~> 1.6", ">= 1.6.9"

  spec.add_runtime_dependency "ffi-yajl", ">= 1.2.0"

  spec.add_runtime_dependency "pry"
  spec.add_runtime_dependency "rb-readline"

  # investigate what it would take to upgrade this...
  spec.add_runtime_dependency "pg", "~> 0.17", ">= 0.17.1"

  spec.add_runtime_dependency "sequel"
  spec.add_runtime_dependency "redis"
  # TODO
  # chef-server-ctl server-admins commands dep, will be removed in server-admins V2
  # https://gist.github.com/tylercloke/a8d4bc1b915b958ac160#version-2
  spec.add_runtime_dependency "rest-client"
  spec.add_runtime_dependency "knife-opc"

  spec.add_runtime_dependency "uuidtools", "~> 2.1", ">= 2.1.3"
  spec.add_runtime_dependency "veil" # todo get latest from https://github.com/chef/chef_secrets.git

  spec.add_runtime_dependency "chef_backup" # Gemfile has us getting latest from git
  spec.add_runtime_dependency "omnibus-ctl" # Gemfile has us getting latest from git

  spec.add_runtime_dependency "chef", "~> 14.3.0"
  spec.add_runtime_dependency "appbundler"

  spec.add_development_dependency "chefstyle"
  spec.add_development_dependency "bundler"
  spec.add_development_dependency "rake"
  spec.add_development_dependency "rspec"

end
