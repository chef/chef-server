name "private-chef-upgrades"

dependencies ["rsync"]

source :path => File.expand_path("files/private-chef-upgrades", Omnibus.root)

build do
  command "mkdir -p #{install_dir}/embedded/upgrades"
  command "#{install_dir}/embedded/bin/rsync --delete -a ./ #{install_dir}/embedded/upgrades/"
end
