name "opscode-reporting"
#version "pc-rel-0.13.0"
version "master" 

dependencies ["erlang", "rsync", "rebar"]

source :git => "git@github.com:opscode/oc_reporting"

relative_path "oc_reporting"

env = {
  "PATH" => "#{install_dir}/embedded/bin:#{ENV["PATH"]}",
  "LD_FLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
  "LD_RUN_PATH" => "#{install_dir}/embedded/lib"
}

build do
  command "make distclean", :env => env
  command "make rel", :env => env
  command "mkdir -p #{install_dir}/embedded/service/opscode-reporting"
  command "#{install_dir}/embedded/bin/rsync -a --delete ./rel/reporting/ #{install_dir}/embedded/service/opscode-reporting/"
  # TODO: git cleanup in opscode-reporting service directory?
  command "rm -rf #{install_dir}/embedded/service/opscode-reporting/log"
end
