name "opscode-certificate"

dependencies ["erlang", "rsync", "openssl"]

source :git => "git@github.com:opscode/opscode-cert-erlang"

relative_path "opscode-certificate"

env = {
  "PATH" => "/opt/opscode/embedded/bin:#{ENV["PATH"]}",
  "LDFLAGS" => "-L/opt/opscode/embedded/lib -I/opt/opscode/embedded/include",
  "LD_RUN_PATH" => "/opt/opscode/embedded/lib"
}

build do
  # TODO: we need to get the app.config file there before we build ...
  # or do we?
  command "make clean", :env => env
  command "make", :env => env
  command "mkdir -p /opt/opscode/embedded/service/opscode-certificate"
  command "/opt/opscode/embedded/bin/rsync -a ./ /opt/opscode/embedded/service/opscode-certificate/"
end
