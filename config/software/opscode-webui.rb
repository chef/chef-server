name "opscode-webui"
version "rel-3.4.3"

dependencies ["ruby", "bundler", "libxml2", "libxslt", "curl", "rsync"]

source :git => "git@github.com:opscode/opscode-webui"

relative_path "opscode-webui"

build do
  bundle "install --without integration_test test dev hosted_chef"
  command "mkdir -p #{install_dir}/embedded/service/opscode-webui"
  command "#{install_dir}/embedded/bin/rsync -a --delete --exclude=.git/*** --exclude=.gitignore ./ #{install_dir}/embedded/service/opscode-webui/"
end
