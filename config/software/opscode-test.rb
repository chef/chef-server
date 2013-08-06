name "opscode-test"
version "0.3.0"

dependency "ruby"
dependency "libxml2"
dependency "rsync"
dependency "bundler"
dependency "postgresql92"

source :git => "git@github.com:opscode/opscode-test"

build do
  bundle "install --without mysql dev --path=/opt/opscode/embedded/service/gem"
  command "mkdir -p #{install_dir}/embedded/service/opscode-test"
  command "#{install_dir}/embedded/bin/rsync -a --delete --exclude=.git/*** --exclude=.gitignore ./ #{install_dir}/embedded/service/opscode-test/"
end
