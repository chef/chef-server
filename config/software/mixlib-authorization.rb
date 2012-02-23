name "mixlib-authorization"
version "pc-rel-1.2.1"

dependencies ["ruby",
              "bundler",
              "postgresql",
              "rsync"]

source :git => "git@github.com:opscode/mixlib-authorization"

relative_path "mixlib-authorization"

bundle_env = {"GEM_HOME" => nil, "GEM_PATH" => nil}

build do
  command "/opt/opscode/embedded/bin/bundle install --without mysql"
  command "mkdir -p /opt/opscode/embedded/service/mixlib-authorization"
  command "/opt/opscode/embedded/bin/rsync -a --delete --exlude=.git/*** --exclude=.gitignore ./ /opt/opscode/embedded/service/mixlib-authorization/"
end
