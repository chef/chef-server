name "mixlib-authorization"

dependencies ["ruby", "bundler", "postgresql"]

source :git => "git@github.com:opscode/mixlib-authorization"

relative_path "mixlib-authorization"

bundle_env = {"GEM_HOME" => nil, "GEM_PATH" => nil}

build do
  command "/opt/opscode/embedded/bin/bundle install --without mysql"
  # TODO: copy the rm, rsync steps from the clojure build
end
