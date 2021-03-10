Gem::Specification.new do |s|
  s.name          = 'oc-chef-pedant'
  s.version       = '2.2.0'
  s.summary       = "Chef Infra Server API Testing Framework"
  s.authors       = ["Chef Software Engineering"]
  s.email         = 'dev@chef.io'
  s.license       = 'Apache-2.0'
  s.require_paths = ['lib', 'spec']
  s.files         = Dir['lib/**/*.rb'] + Dir['spec/**/*_spec.rb']
  s.homepage      = 'https://chef.io'

  s.bindir        = 'bin'
  s.executables   = ['oc-chef-pedant']

  s.add_dependency('rspec', '~> 3.2')
  s.add_dependency('activesupport', '>= 4.2.7.1', '< 7.0') # For active_support/concern
  s.add_dependency('mixlib-authentication', '> 1.4', '< 3.0')
  s.add_dependency('mixlib-config', '>= 2', '< 4')
  s.add_dependency('mixlib-shellout', '>= 1.1')
  s.add_dependency('rest-client', '>= 1.6')
  s.add_dependency('rspec_junit_formatter', '~> 0.2')
  s.add_dependency('net-http-spy', '~> 0.2')
  s.add_dependency('uuidtools', '~> 2.0')
  s.add_dependency('erubis', '~> 2.7')
  s.add_dependency('rspec-rerun', '~> 1.0')
end
