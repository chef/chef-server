name "opscode-test"

dependencies ["ruby", "libxml2"]

source :git => "git@github.com:opscode/opscode-test"

bundle_env = {"GEM_HOME" => nil, "GEM_PATH" => nil}

build do
  command "/opt/opscode/embedded/bin/bundle install --without mysql dev", :env => bundle_env
  # TODO: copy the rm, rsync steps from the clojure build
end
