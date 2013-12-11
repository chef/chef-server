Gem::Specification.new do |s|
  s.name          = 'oc-chef-pedant'
  s.version       = '1.0.23'
  s.date          = '2013-10-31'
  s.summary       = "Opscode Private Chef API Testing Framework"
  s.authors       = ["Opscode Software Engineering"]
  s.email         = 'dev@opscode.com'
  s.require_paths = ['lib', 'spec']
  s.files         = Dir['lib/**/*.rb'] + Dir['spec/**/*_spec.rb']
  s.homepage      = 'http://opscode.com'

  s.bindir        = 'bin'
  s.executables   = ['oc-chef-pedant']

  s.add_dependency('chef-pedant', '>= 1.0.0')
end
