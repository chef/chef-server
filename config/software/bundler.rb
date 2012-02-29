name "bundler"
version "1.0.22"

dependencies ["rubygems"]

build do
  gem "install bundler --no-rdoc --no-ri -v '#{version}'"
end

