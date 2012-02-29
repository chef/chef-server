name "opscode-test"
version "rel-0.2.0"

dependencies ["ruby", "libxml2", "rsync", "bundler"]

source :git => "git@github.com:opscode/opscode-test"

build do
  bundle "install --without mysql dev"
  command "mkdir -p #{install_dir}/embedded/service/opscode-test"
  command "#{install_dir}/embedded/bin/rsync -a --delete --exclude=.git/*** --exclude=.gitignore ./ #{install_dir}/embedded/service/opscode-test/"
end
