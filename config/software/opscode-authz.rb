name "opscode-authz"
version "pc-rel-0.3.0"

dependencies ["erlang", "rsync"]

source :git => "git@github.com:opscode/opscode-authz"

relative_path "opscode-authz"

env = {
  "PATH" => "#{install_dir}/embedded/bin:#{ENV["PATH"]}",
  "LD_FLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
  "LD_RUN_PATH" => "#{install_dir}/embedded/lib"
}

build do
  command "make distclean", :env => env
  command "make rel", :env => env
  command "mkdir -p #{install_dir}/embedded/service/opscode-authz"
  command "#{install_dir}/embedded/bin/rsync -a ./rel/authz #{install_dir}/embedded/service/opscode-authz/"
  command "rm -rf #{install_dir}/embedded/service/opscode-authz/log"
end
