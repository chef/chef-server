Gem::Specification.new do |s|
  s.name          = 'oc-bifrost-pedant'
  s.version       = '0.0.1'
  s.date          = '2013-01-24'
  s.summary       = "Opscode Chef API Testing Framework"
  s.authors       = ["Opscode Software Engineering"]
  s.email         = 'dev@chef.io'
  s.require_paths = ['lib', 'spec']
  s.files         = Dir['lib/**/*'] + Dir['spec/**/*'] + Dir['bin/*'] + Dir['fixtures/**/*']
  s.homepage      = 'http://chef.io'

  s.bindir        = 'bin'
  s.executables   = ['oc-bifrost-pedant']

  s.add_dependency('rspec', '~> 2.11.0')
  s.add_dependency('activesupport', '~> 3.2.8') # For active_support/concern
  s.add_dependency('mixlib-config', '~> 1.1.2')
  s.add_dependency('rest-client', '~> 1.6.7')
  s.add_dependency('rspec_junit_formatter', '~> 0.1.1')
  s.add_dependency('net-http-spy', '~> 0.2.1')
  s.add_dependency('rspec-rerun', '= 0.1.1')
end
