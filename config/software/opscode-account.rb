name "opscode-account"
default_version "rel-1.30.7.1"

dependencies ["ruby",
              "bundler",
              "postgresql",
              "rsync"]

source :git => "git@github.com:opscode/opscode-account"

relative_path "opscode-account"

build do
  bundle "install --without mysql test --path=/opt/opscode/embedded/service/gem"
  command "mkdir -p #{install_dir}/embedded/service/opscode-account"
  command "#{install_dir}/embedded/bin/rsync -a --delete --exclude=.git/*** --exclude=.gitignore ./ #{install_dir}/embedded/service/opscode-account/"
end
