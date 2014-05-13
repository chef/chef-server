name "opscode-test"
default_version "pc-rel-0.2.0.1"

dependencies ["ruby", "libxml2", "rsync", "bundler"]

source :git => "git@github.com:opscode/opscode-test"

build do
  bundle "install --without mysql dev --path=/opt/opscode/embedded/service/gem"
  command "mkdir -p #{install_dir}/embedded/service/opscode-test"
  command "#{install_dir}/embedded/bin/rsync -a --delete --exclude=.git/*** --exclude=.gitignore ./ #{install_dir}/embedded/service/opscode-test/"
end
