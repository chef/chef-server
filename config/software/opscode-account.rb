name "opscode-account"
version "pc-rel-1.11.3.3"

dependencies ["ruby",
              "bundler",
              "postgresql",
              "rsync"]

source :git => "git@github.com:opscode/opscode-account"

relative_path "opscode-account"

bundle_env = {"GEM_PATH" => nil, "GEM_HOME" => nil}

build do
  command "/opt/opscode/embedded/bin/bundle install --without mysql test", :env => bundle_env
  command "mkdir -p /opt/opscode/embedded/service/opscode-account"
  command "/opt/opscode/embedded/bin/rsync -a --delete --exclude=.git/*** --exclude=.gitignore ./ /opt/opscode/embedded/service/opscode-account/"
end
