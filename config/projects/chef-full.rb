name "chef-full"

install_path    "/opt/chef"
build_version   "0.10.8"
build_iteration "4"

if ENV['chef_git']
  build_iteration << ".#{ENV['chef_git']}"
  dependencies ["chef-git"]
else
  dependencies ["chef-gem"]
end

