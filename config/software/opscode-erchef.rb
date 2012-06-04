name "opscode-erchef"
version "master"

dependencies ["erlang", "rsync"]

source :git => "git@github.com:opscode/opscode-chef-api-erlang"

relative_path "opscode-chef-api-erlang"

env = {
  "PATH" => "#{install_dir}/embedded/bin:#{ENV["PATH"]}",
  "LD_FLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
  "LD_RUN_PATH" => "#{install_dir}/embedded/lib"
}

build do
  command "make distclean", :env => env
  command "make rel", :env => env
  command "mkdir -p #{install_dir}/embedded/service/opscode-erchef"
  command "#{install_dir}/embedded/bin/rsync -a --delete ./rel/erchef/ #{install_dir}/embedded/service/opscode-erchef/"
  # TODO: git cleanup in opscode-erchef service directory
  command "rm -rf #{install_dir}/embedded/service/opscode-erchef/log"
end
