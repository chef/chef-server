name "opscode-authz"
default_version "pc-rel-0.3.4"

dependencies ["erlang", "rsync"]

source :url => "http://there-is-no-url.file-only-in-cache.org/opscode-authz-#{default_version}.tar.gz",
       :md5 => "5499cd5e54c686c97d117f29265960d8"

relative_path "opscode-authz"

env = {
  "PATH" => "#{install_dir}/embedded/bin:#{ENV["PATH"]}",
  "LDFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
  "LD_RUN_PATH" => "#{install_dir}/embedded/lib"
}

build do
  command "make rel", :env => env
  command "mkdir -p #{install_dir}/embedded/service/opscode-authz"
  command "#{install_dir}/embedded/bin/rsync -a ./ #{install_dir}/embedded/service/opscode-authz/"
  command "rm -rf #{install_dir}/embedded/service/opscode-authz/rel/authz/log"
end
