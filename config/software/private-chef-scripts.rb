name "private-chef-scripts"

dependency "rsync"

source :path => File.expand_path("files/private-chef-scripts", Config.project_root)

build do
  command "mkdir -p #{install_dir}/bin"
  command "#{install_dir}/embedded/bin/rsync -a ./ #{install_dir}/bin/"
end
