name "opscode-chef"
version "rel-2.8.6"

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

build do
  bundle "install --without mysql integration_test dev --path=/opt/opscode/embedded/service/gem"
  command "mkdir -p #{install_dir}/embedded/service/opscode-chef"
  command "#{install_dir}/embedded/bin/rsync -a --delete --delete-excluded --exclude=.git/*** --exclude=.gitignore --exclude=chef-server-webui/mockups/*** ./ #{install_dir}/embedded/service/opscode-chef/"
end
