name "private-chef-administration"
version "rel-1.4.0"

dependency "rsync"

source :git => "git@github.com:opscode/private-chef-administration"

relative_path "private-chef-administration"

build do
  command "make html"
  command "mkdir -p #{install_dir}/docs"
  command "/opt/opscode/embedded/bin/rsync -a --delete ./build/html/ #{install_dir}/docs"
end
