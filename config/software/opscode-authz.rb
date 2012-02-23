name "opscode-authz"
version "rel-0.2.3"

dependencies ["erlang", "rsync"]

source :git => "git@github.com:opscode/opscode-authz"

relative_path "opscode-authz"

env = { "PATH" => "/opt/opscode/embedded/bin:#{ENV["PATH"]}" }

build do
  command "make clean", :env => env
  command "make", :env => env
  command "mkdir -p /opt/opscode/embedded/service/opscode-authz"
  command "/opt/opscode/embedded/bin/rsync -a ./ /opt/opscode/embedded/service/opscode-authz/"
end
