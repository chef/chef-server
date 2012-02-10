name "opscode-platform-debug"

dependencies ["ruby", "bundler", "postgresql"]

source :git => "git@github.com:opscode/opscode-platform-debug"

relative_path "opscode-plaform-debug"

bundle_env = {"GEM_HOME" => nil, "GEM_PATH" => nil}

build do
  # TODO: do we need to / should we bundle install orgmapper here?
  # TODO: copy the rm, rsync steps from the clojure build
end
