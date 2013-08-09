name "opscode-platform-debug"
version "rel-0.4.3"

dependency "ruby"
dependency "bundler"
dependency "postgresql92"
dependency "rsync"

source :git => "git@github.com:opscode/opscode-platform-debug"

relative_path "opscode-platform-debug"

orgmapper_dir = "#{project_dir}/orgmapper"

# Since this project pulls in the pg gem (or depends on something that
# does) we need to have the pg_config binary on the PATH so the
# correct library and header locations can be found
env = {
  'PATH' => "#{install_dir}/embedded/bin:#{ENV['PATH']}"
}

build do
  # bundle install orgmapper
  bundle "install --path=/opt/opscode/embedded/service/gem", :cwd => orgmapper_dir, :env => env
  command "mkdir -p #{install_dir}/embedded/service/opscode-platform-debug"
  command "#{install_dir}/embedded/bin/rsync -a --delete --exclude=.git/*** --exclude=.gitignore ./ #{install_dir}/embedded/service/opscode-platform-debug/"
end
