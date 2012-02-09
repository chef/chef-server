name "opscode-expander"

dependencies ["ruby", "bundler"]

source :git => "git@github.com:opscode/opscode-expander"

relative_path "opscode-expander"

bundle_env = {"GEM_PATH" => nil, "GEM_HOME" => nil}

build do
  command "/opt/opscode/embedded/bin/bundle install --without mysql"
  # TODO: copy the rm, rsync steps from the clojure build
end
