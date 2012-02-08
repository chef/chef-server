name "unicorn"

dependencies ["rubygems"]

env = { "GEM_HOME" => nil, "GEM_PATH" => nil }

build do
  command "/opt/opscode/embedded/bin/gem install unicorn --no-rdoc --no-ri", :env => env
end
