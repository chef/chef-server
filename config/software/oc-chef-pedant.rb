name "oc-chef-pedant"
version "rel-1.0.0"

dependencies ["ruby",
              "bundler",
              "rsync"]

source :git => "git@github.com:opscode/oc-chef-pedant.git"

relative_path "oc-chef-pedant"

build do
  bundle "install --path=#{install_dir}/embedded/service/gem"
  command "mkdir -p #{install_dir}/embedded/service/oc-chef-pedant"
  command "#{install_dir}/embedded/bin/rsync -a --delete --exclude=.git/*** --exclude=.gitignore ./ #{install_dir}/embedded/service/oc-chef-pedant/"
end
