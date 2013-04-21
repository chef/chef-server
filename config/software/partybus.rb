name "partybus"

dependency "rsync"
dependency "bundler"
dependency "postgresql"

source :path => File.expand_path("partybus", Omnibus.project_root)

build do
  command "mkdir -p #{install_dir}/embedded/service/partybus"
  bundle "install --without mysql --path=/opt/opscode/embedded/service/gem"
  command "#{install_dir}/embedded/bin/rsync --delete -a ./ #{install_dir}/embedded/service/partybus"
end
