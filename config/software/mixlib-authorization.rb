name "mixlib-authorization"
version "pc-rel-1.2.1"

dependencies ["ruby",
              "bundler",
              "postgresql",
              "rsync"]

source :git => "git@github.com:opscode/mixlib-authorization"

relative_path "mixlib-authorization"

bundle_env = {"GEM_HOME" => nil, "GEM_PATH" => nil}

build do
  command "#{install_dir}/embedded/bin/bundle install --without mysql"
  command "mkdir -p #{install_dir}/embedded/service/mixlib-authorization"
  command "#{install_dir}/embedded/bin/rsync -a --delete --exlude=.git/*** --exclude=.gitignore ./ #{install_dir}/embedded/service/mixlib-authorization/"
end
