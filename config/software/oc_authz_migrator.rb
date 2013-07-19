name "oc_authz_migrator"
version "master"

dependencies ["erlang", "rebar", "rsync"]

source :git => "git@github.com:opscode/oc_authz_migrator"

relative_path "oc_authz_migrator"

env = {
  "PATH" => "#{install_dir}/embedded/bin:#{ENV["PATH"]}",
  "LDFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
  "CFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
  "LD_RUN_PATH" => "#{install_dir}/embedded/lib"
}

build do
  command "make distclean", :env => env
  command "make compile", :env => env
  command "mkdir -p #{install_dir}/embedded/service/oc_authz_migrator"
  command "#{install_dir}/embedded/bin/rsync -a --delete . #{install_dir}/embedded/service/oc_authz_migrator/"
end
