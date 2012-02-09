name "opscode-account"

dependencies ["ruby",
              "bundler",
              "postgresql"]

source :git => "git@github.com:opscode/opscode-account"

relative_path "opscode-account"

bundle_env = {"GEM_PATH" => nil, "GEM_HOME" => nil}

build do
  command "/opt/opscode/embedded/bin/bundle install --without mysql test", :env => bundle_env
  # TODO: copy the rm, rsync steps from the clojure build
end
