Gem::Specification.new do |s|
  s.name        = 'dvm'
  s.version     = '100.0.0'
  s.date        = '2015-03-18'
  s.summary     = "chef-server development enabler"
  s.description = "A controller for your friendly local chef-server dev vm. Don't leave home without it!"
  s.authors     = ["Marc A. Paradise", "Chef Software, Inc"]
  s.email       = 'marc@chef.io'
  s.files       = Dir.glob("{bin,lib}/**/*") + [ "parse.es" ]
  s.executables << 'dvm'
  s.license       = 'Apache'
  s.add_dependency "highline"
  s.add_dependency "thor"
  s.add_dependency "mixlib-shellout"
  s.add_dependency "deep_merge"
end
