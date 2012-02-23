name "opscode-test"
version "rel-0.2.0"

dependencies ["ruby", "libxml2", "rsync"]

source :git => "git@github.com:opscode/opscode-test"

bundle_env = {"GEM_HOME" => nil, "GEM_PATH" => nil}

build do
  command "/opt/opscode/embedded/bin/bundle install --without mysql dev", :env => bundle_env
  command "mkdir -p /opt/opscode/embedded/service/opscode-test"
  command "/opt/opscode/embedded/bin/rsync -a --delete --exclude=.git/*** --exclude=.gitignore ./ /opt/opscode/embedded/service/opscode-test/"
end
