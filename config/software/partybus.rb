name "partybus"

dependency "rsync"
dependency "bundler"
dependency "postgresql92"

source :path => File.expand_path("partybus", Omnibus.project_root)

# Since this project pulls in the pg gem (or depends on something that
# does) we need to have the pg_config binary on the PATH so the
# correct library and header locations can be found
env = {
  'PATH' => "#{install_dir}/embedded/bin:#{ENV['PATH']}"
}

build do
  command "mkdir -p #{install_dir}/embedded/service/partybus"
  bundle "install --without mysql --path=/opt/opscode/embedded/service/gem", :env => env
  command "#{install_dir}/embedded/bin/rsync --delete -a ./ #{install_dir}/embedded/service/partybus"
end
