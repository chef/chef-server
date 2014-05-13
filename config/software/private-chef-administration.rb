name "private-chef-administration"
default_version "rel-1.4.0"

dependency "sphinx"
dependency "pygments"
dependency "rsync"

source :git => "git@github.com:opscode/private-chef-administration"

relative_path "private-chef-administration"

env = {
  "PATH" => "#{install_dir}/embedded/bin:#{ENV['PATH']}"
}

build do
  command "make html", :env => env
  command "mkdir -p #{install_dir}/docs"
  command "/opt/opscode/embedded/bin/rsync -a --delete ./build/html/ #{install_dir}/docs"
end
