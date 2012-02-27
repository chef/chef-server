name "opscode-account"
version "pc-rel-1.11.3.3"

dependencies ["ruby",
              "bundler",
              "postgresql",
              "rsync"]

source :git => "git@github.com:opscode/opscode-account"

relative_path "opscode-account"

bundle_env = {"GEM_PATH" => nil, "GEM_HOME" => nil}

build do
  command "#{install_dir}/embedded/bin/bundle install --without mysql test", :env => bundle_env
  command "mkdir -p #{install_dir}/embedded/service/opscode-account"
  command "#{install_dir}/embedded/bin/rsync -a --delete --exclude=.git/*** --exclude=.gitignore ./ #{install_dir}/embedded/service/opscode-account/"
end
