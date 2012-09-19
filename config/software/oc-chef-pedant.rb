name "oc-chef-pedant"
version "master"

dependencies ["ruby",
              "bundler",
              "rsync"]

source :git => "git@github.com:opscode/oc-chef-pedant"

relative_path "oc-chef-pedant"

build do
  bundle "install --path=/opt/opscode/embedded/service/gem"
  command "mkdir -p #{install_dir}/embedded/service/oc-chef-pedant"
  command "#{install_dir}/embedded/bin/rsync -a --delete --exclude=.git/*** --exclude=.gitignore ./ #{install_dir}/embedded/service/oc-chef-pedant/"
end
