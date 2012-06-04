name "opscode-pedant"
version "master"

dependencies ["ruby",
              "bundler",
              "rsync"]

source :git => "git@github.com:opscode/opscode-pedant"

relative_path "opscode-pedant"

build do
  bundle "install --path=/opt/opscode/embedded/service/gem"
  command "mkdir -p #{install_dir}/embedded/service/opscode-pedant"
  command "#{install_dir}/embedded/bin/rsync -a --delete --exclude=.git/*** --exclude=.gitignore ./ #{install_dir}/embedded/service/opscode-pedant/"
end
