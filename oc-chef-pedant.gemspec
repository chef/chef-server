Gem::Specification.new do |s|
  s.name          = 'oc-chef-pedant'
  s.version       = '1.0.29.5'
  s.date          = '2014-10-22'
  s.summary       = "Opscode Private Chef API Testing Framework"
  s.authors       = ["Chef Software Engineering"]
  s.email         = 'dev@getchef.com'
  s.require_paths = ['lib', 'spec']
  s.files         = Dir['lib/**/*.rb'] + Dir['spec/**/*_spec.rb']
  s.homepage      = 'http://getchef.com'

  s.bindir        = 'bin'
  s.executables   = ['oc-chef-pedant']

  s.add_dependency('chef-pedant', '>= 1.0.0')
end
