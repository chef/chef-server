Gem::Specification.new do |s|
  s.name          = 'oc-chef-pedant'
  s.version       = '2.0.0'
  s.date          = '2015-02-27'
  s.summary       = "Enterprise Chef API Testing Framework"
  s.authors       = ["Chef Software Engineering"]
  s.email         = 'dev@chef.io'
  s.require_paths = ['lib', 'spec']
  s.files         = Dir['lib/**/*.rb'] + Dir['spec/**/*_spec.rb']
  s.homepage      = 'http://chef.io'

  s.bindir        = 'bin'
  s.executables   = ['oc-chef-pedant']

  s.add_dependency('rspec', '~> 3.2.0')
  s.add_dependency('activesupport', '~> 3.2.8') # For active_support/concern
  s.add_dependency('mixlib-authentication', '~> 1.3.0')
  s.add_dependency('mixlib-config', '~> 2.0')
  s.add_dependency('mixlib-shellout', '>= 1.1')
  s.add_dependency('rest-client', '>= 1.6.7')
  s.add_dependency('rspec_junit_formatter', '~> 0.2.0')
  s.add_dependency('net-http-spy', '~> 0.2.1')
  s.add_dependency('erubis', '~> 2.7.0')
  s.add_dependency('rspec-rerun', '= 0.1.1')
  s.add_dependency('rspec-legacy_formatters', '~> 1.0')
end
