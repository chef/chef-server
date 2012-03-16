name "private-chef-scripts"

dependencies [ "rsync" ]

source :path => File.expand_path("files/private-chef-scripts", Omnibus.root)

build do
  command "mkdir -p #{install_dir}/embedded/bin"
  command "#{install_dir}/embedded/bin/rsync -a ./ #{install_dir}/bin/"
end
