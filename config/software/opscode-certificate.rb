name "opscode-certificate"
version "rel-0.1.2"

dependency "erlang"
dependency "rsync"

source :git => "git@github.com:opscode/opscode-cert-erlang"

relative_path "opscode-certificate"

env = {
  "PATH" => "#{install_dir}/embedded/bin:#{ENV["PATH"]}",
  "LDFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
  "CFLAGS" => "-I#{install_dir}/embedded/include",
  "LD_RUN_PATH" => "#{install_dir}/embedded/lib"
}

build do
  # TODO: we need to get the app.config file there before we build ...
  # or do we?
  command "make clean", :env => env
  command "make", :env => env
  command "mkdir -p #{install_dir}/embedded/service/opscode-certificate"
  command "#{install_dir}/embedded/bin/rsync -a ./ #{install_dir}/embedded/service/opscode-certificate/"
end
