name "opscode-erchef"
version "pc-rc-0.8.0-tag"

dependencies ["erlang", "rsync"]

source :git => "git@github.com:opscode/opscode-chef-api-erlang"

relative_path "opscode-chef-api-erlang"

env = {
  "PATH" => "/opt/opscode/embedded/bin:#{ENV["PATH"]}",
  "LD_FLAGS" => "-L/opt/opscode/embedded/lib -I/opt/opscode/embedded/include",
  "LD_RUN_PATH" => "/opt/opscode/embedded/lib"
}

build do
  command "make distclean", :env => env
  command "make rel", :env => env
  command "mkdir -p /opt/opscode/embedded/service/opscode-erchef"
  command "/opt/opscode/embedded/bin/rsync -a ./rel/erchef/ /opt/opscode/embedded/service/opscode-erchef/"
  # TODO: git cleanup in opscode-erchef service directory
end
