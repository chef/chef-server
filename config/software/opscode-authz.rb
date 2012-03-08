name "opscode-authz"
version "pc-rel-0.3.0"

dependencies ["erlang", "rsync"]

source :git => "git@github.com:opscode/opscode-authz"

relative_path "opscode-authz"

env = { "PATH" => "#{install_dir}/embedded/bin:#{ENV["PATH"]}" }

build do
  command "make clean", :env => env
  command "make", :env => env
  command "mkdir -p #{install_dir}/embedded/service/opscode-authz"
  command "#{install_dir}/embedded/bin/rsync -a ./ #{install_dir}/embedded/service/opscode-authz/"
end
