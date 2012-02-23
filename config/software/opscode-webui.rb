name "opscode-webui"
version "rel-3.4.3"

dependencies ["ruby", "bundler", "libxml2", "libxslt", "curl", "rsync"]

source :git => "git@github.com:opscode/opscode-webui"

relative_path "opscode-webui"

bundle_env = { "GEM_PATH" => nil, "GEM_HOME" => nil  }

build do
  command "/opt/opscode/embedded/bin/bundle install --without integration_test test dev hosted_chef", :env => bundle_env
  command "mkdir -p /opt/opscode/embedded/service/opscode-webui"
  command "/opt/opscode/embedded/bin/rsync -a --delete --exclude=.git/*** --exclude=.gitignore ./ /opt/opscode/embedded/service/opscode-webui/"
end
