name "opscode-webui"

dependencies ["ruby", "bundler", "libxml2", "libxslt", "curl"]

source :git => "git@github.com:opscode/opscode-webui"

relative_path "opscode-webui"

bundle_env = { "GEM_PATH" => nil, "GEM_HOME" => nil  }

build do
  command "/opt/opscode/embedded/bin/bundle install --without integration_test test dev hosted_chef", :env => bundle_env
  # TODO: copy the rm, rsync steps from the clojure build
end
