name "opscode-chef-mover"
default_version "2.2.17"

dependency "erlang"
dependency "rebar"
dependency "rsync"

source :git => "git@github.com:opscode/chef-mover"

relative_path "opscode-chef-mover"

env = {
  "PATH" => "#{install_dir}/embedded/bin:#{ENV["PATH"]}",
  "LDFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
  "CFLAGS" => "-L#{install_dir}/embedded/lib -I#{install_dir}/embedded/include",
  "LD_RUN_PATH" => "#{install_dir}/embedded/lib"
}

build do
  command "make distclean", :env => env
  command "make rel", :env => env
  command "mkdir -p #{install_dir}/embedded/service/opscode-chef-mover"
  command "#{install_dir}/embedded/bin/rsync -a --delete ./rel/mover/ #{install_dir}/embedded/service/opscode-chef-mover/"
  command "rm -rf #{install_dir}/embedded/service/opscode-chef-mover/log"
  command "mkdir -p #{install_dir}/embedded/service/opscode-chef-mover/scripts"
  command "cp scripts/migrate #{install_dir}/embedded/service/opscode-chef-mover/scripts"
  command "chmod ugo+x #{install_dir}/embedded/service/opscode-chef-mover/scripts/migrate"
  command "cp scripts/check_logs.rb #{install_dir}/embedded/service/opscode-chef-mover/scripts"
  command "chmod ugo+x #{install_dir}/embedded/service/opscode-chef-mover/scripts/check_logs.rb"
end
