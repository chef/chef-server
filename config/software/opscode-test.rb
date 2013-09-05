name "opscode-test"
version "0.3.1"

dependency "ruby"
dependency "libxml2"
dependency "rsync"
dependency "bundler"
dependency "postgresql92"

source :git => "git@github.com:opscode/opscode-test"

# Since this project pulls in the pg gem (or depends on something that
# does) we need to have the pg_config binary on the PATH so the
# correct library and header locations can be found
env = {
  'PATH' => "#{install_dir}/embedded/bin:#{ENV['PATH']}"
}

build do
  bundle "install --without dev --path=/opt/opscode/embedded/service/gem", :env => env
  command "mkdir -p #{install_dir}/embedded/service/opscode-test"
  command "#{install_dir}/embedded/bin/rsync -a --delete --exclude=.git/*** --exclude=.gitignore ./ #{install_dir}/embedded/service/opscode-test/"
end
