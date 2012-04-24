name "partybus"

dependencies ["rsync"]

source :path => File.expand_path("partybus", Omnibus.root)

build do
  command "mkdir -p #{install_dir}/embedded/service/partybus"
  command "#{install_dir}/embedded/bin/rsync --delete -a ./ #{install_dir}/embedded/service/partybus"
end
