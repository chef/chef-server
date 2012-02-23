name "opscode-chef"
version "pc-rel-2.4.0.4"

dependencies ["ruby",
              "bundler",
              "gecode",
              "libxml2",
              "libxslt",
              "postgresql", # TODO: how do we install just the client library?
              "curl",
              "rsync"]

source :git => "git@github.com:opscode/opscode-chef"

relative_path "opscode-chef"

bundle_env = {"GEM_PATH" => nil, "GEM_HOME" => nil}

build do
  command "/opt/opscode/embedded/bin/bundle install --without mysql integration_test dev", :env => bundle_env
  command "mkdir -p /opt/opscode/embedded/service/opscode-chef"
  command "/opt/opscode/embedded/bin/rsync -a --delete --exclude=.git/*** --exclude=.gitignore ./ /opt/opscode/embedded/service/opscode-chef/"
end
