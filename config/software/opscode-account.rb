name "opscode-account"

dependencies ["ruby",
              "bundler",
              "postgresql"]

source :git => "git@github.com:opscode/opscode-account"

relative_path "opscode-account"

bundle_env = {"GEM_PATH" => nil, "GEM_HOME" => nil}

build do
  command "/opt/opscode/embedded/bin/bundle install --without mysql test", :env => bundle_env
  command "git log -1 --format=%H > .githash"
  command "mkdir -p /opt/opscode/embedded/service/opscode-account"
  command "rsync -a --delete --exclude=.git/*** --exclude=.gitignore ./ /opt/opscode/embedded/service/opscode-account/"
end
