name "opscode-test"
version "rel-0.2.0"

dependencies ["ruby", "libxml2", "rsync"]

source :git => "git@github.com:opscode/opscode-test"

bundle_env = {"GEM_HOME" => nil, "GEM_PATH" => nil}

build do
  command "#{install_dir}/embedded/bin/bundle install --without mysql dev", :env => bundle_env
  command "mkdir -p #{install_dir}/embedded/service/opscode-test"
  command "#{install_dir}/embedded/bin/rsync -a --delete --exclude=.git/*** --exclude=.gitignore ./ #{install_dir}/embedded/service/opscode-test/"
end
