name "opscode-webui2"
version "release"

dependency "bundler"
dependency "libxml2"
dependency "libxslt"
dependency "rsync"
dependency "zlib"

source :git => "git@github.com:opscode/opscode-webui2"

relative_path "opscode-webui2"

build do
  bundle  "install --without development test --path=/opt/opscode/embedded/service/gem"
  rake    "assets:precompile"
  command "mkdir -p #{install_dir}/embedded/service/opscode-webui2"
  command "#{install_dir}/embedded/bin/rsync -a --delete --delete-excluded --exclude=.git/*** --exclude=.gitignore --exclude=prototype/*** ./ #{install_dir}/embedded/service/opscode-webui2/"
end
