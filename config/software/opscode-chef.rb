name "opscode-chef"

dependencies ["ruby",
              "bundler",
              "gecode",
              "libxml2",
              "libxslt",
              "postgresql", # TODO: how do we install just the client library?
              "curl"]

source :git => "git@github.com:opscode/opscode-chef"

relative_path "opscode-chef"

bundle_env = {"GEM_PATH" => nil, "GEM_HOME" => nil}

build do
  command "/opt/opscode/embedded/bin/bundle install --without mysql integration_test dev", :env => bundle_env
  # TODO: copy the rm, rsync steps from the clojure build
end
