name "oc_bifrost"
version "1.2.2"

dependencies ["erlang", "rebar", "rsync"]

source :git => "git@github.com:opscode/oc_bifrost"

relative_path "oc_bifrost"

env = {
  "PATH" => "#{install_dir}/embedded/bin:#{ENV["PATH"]}",
  "LDFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
  "CFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
  "LD_RUN_PATH" => "#{install_dir}/embedded/lib"
}

build do
  command "make distclean", :env => env
  command "make rel", :env => env
  command "mkdir -p #{install_dir}/embedded/service/bifrost"
  command "#{install_dir}/embedded/bin/rsync -a --delete ./rel/oc_bifrost/ #{install_dir}/embedded/service/bifrost/"
  # TODO: git cleanup in oc_bifrost service directory
  command "rm -rf #{install_dir}/embedded/service/bifrost/log"
  command "#{install_dir}/embedded/bin/rsync -a --delete ./schema/sql/ #{install_dir}/embedded/service/bifrost/db/"
end
