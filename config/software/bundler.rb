name "bundler"
version "1.0.22"

dependencies ["rubygems"]

env = { "GEM_HOME" => nil, "GEM_PATH" => nil }

build do
  command "#{install_dir}/embedded/bin/gem install bundler --no-rdoc --no-ri -v '#{version}'", :env => env
end

