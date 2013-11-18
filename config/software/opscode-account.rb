name "opscode-account"
version "hh/ipv6"

dependency "ruby"
dependency "bundler"
dependency "postgresql92"
dependency "rsync"

source :git => "git@github.com:opscode/opscode-account"

relative_path "opscode-account"

# Since this project pulls in the pg gem (or depends on something that
# does) we need to have the pg_config binary on the PATH so the
# correct library and header locations can be found
env = {
  'PATH' => "#{install_dir}/embedded/bin:#{ENV['PATH']}"
}

build do
  bundle "install --without test --path=/opt/opscode/embedded/service/gem", :env => env
  command "mkdir -p #{install_dir}/embedded/service/opscode-account"
  command "#{install_dir}/embedded/bin/rsync -a --delete --exclude=.git/*** --exclude=.gitignore ./ #{install_dir}/embedded/service/opscode-account/"
end
