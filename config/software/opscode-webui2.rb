name "opscode-webui2"
version "master"

dependency "bundler"
dependency "libxml2"
dependency "libxslt"
dependency "rsync"
dependency "zlib"

source :git => "git@github.com:opscode/opscode-webui2"

relative_path "opscode-webui2"

build do
  bundle 'install --without development test --path=/opt/opscode/embedded/service/gem'
  # Stub this YAML file to allow the rake task to run.
  command "echo 'production:' > #{source_dir}/opscode-webui2/config/opscode_platform.yml"
  rake 'assets:precompile'
  command "mkdir -p #{install_dir}/embedded/service/opscode-webui2"
  command "#{install_dir}/embedded/bin/rsync -a --delete --delete-excluded --exclude=.git/*** --exclude=.gitignore --exclude=prototype/*** ./ #{install_dir}/embedded/service/opscode-webui2/"
end
