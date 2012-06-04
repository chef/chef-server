name "mixlib-authorization"
version "master"

dependencies ["ruby",
              "bundler",
              "postgresql",
              "rsync"]

source :git => "git@github.com:opscode/mixlib-authorization"

relative_path "mixlib-authorization"

build do
  bundle "install --without mysql --path=/opt/opscode/embedded/service/gem"
  command "mkdir -p #{install_dir}/embedded/service/mixlib-authorization"
  command "#{install_dir}/embedded/bin/rsync -a --delete --exclude=.git/*** --exclude=.gitignore ./ #{install_dir}/embedded/service/mixlib-authorization/"
end
