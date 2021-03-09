Gem::Specification.new do |s|
  s.name          = 'oc-bifrost-pedant'
  s.version       = '0.0.1'
  s.summary       = "Chef Server API Testing Framework"
  s.authors       = ["Chef Software Engineering"]
  s.email         = 'dev@chef.io'
  s.require_paths = ['lib', 'spec']
  s.files         = Dir['lib/**/*'] + Dir['spec/**/*'] + Dir['bin/*'] + Dir['fixtures/**/*']
  s.homepage      = 'https://chef.io'

  s.bindir        = 'bin'
  s.executables   = ['oc-bifrost-pedant']

  s.add_dependency('rspec', '~> 2.11.0')
  s.add_dependency('activesupport', '~> 3.2.8') # For active_support/concern
  s.add_dependency('mixlib-config', '~> 1.1.2')
  s.add_dependency('rest-client', '~> 1.8.0')
  s.add_dependency('rspec_junit_formatter', '>= 0.1.1', '< 0.5.0')
  s.add_dependency('net-http-spy', '~> 0.2.1')
  s.add_dependency('rspec-rerun', '= 0.1.1')
end
