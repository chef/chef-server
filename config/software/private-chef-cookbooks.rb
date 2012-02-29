name "private-chef-cookbooks"

build do
  cookbook_dir = File.expand_path("files/private-chef-cookbooks", Omnibus.root)
  command "mkdir -p #{install_dir}/embedded/cookbooks"
  command "cp -ra #{cookbook_dir}/* #{install_dir}/embedded/cookbooks/"
  command "ln -sf #{install_dir}/embedded/cookbooks/bin/private-chef-ctl #{install_dir}/bin/private-chef-ctl"
end
