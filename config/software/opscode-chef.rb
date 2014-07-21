name "opscode-chef"
default_version "rel-2.8.7"

dependency 'ruby'
dependency 'bundler'
dependency 'gecode'
dependency 'libxml2'
dependency 'libxslt'
dependency 'postgresql' # TODO: how do we install just the client library?
dependency 'curl'
dependency 'rsync'
dependency 'libffi'

source :git => "git@github.com:opscode/opscode-chef"

relative_path "opscode-chef"

env = {
  "CFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
  "LDFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include"
}

build do
  bundle "install --without mysql integration_test dev --path=/opt/opscode/embedded/service/gem", :env => env
  command "mkdir -p #{install_dir}/embedded/service/opscode-chef"
  command "#{install_dir}/embedded/bin/rsync -a --delete --delete-excluded --exclude=.git/*** --exclude=.gitignore --exclude=chef-server-webui/mockups/*** ./ #{install_dir}/embedded/service/opscode-chef/"
end
