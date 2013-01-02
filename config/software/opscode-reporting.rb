name "opscode-reporting"
version "rel-1.1.2" 

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
  command "rm -rf #{install_dir}/embedded/service/opscode-reporting/log"

  # DB
  command "mkdir -p #{install_dir}/embedded/service/opscode-reporting/db"
  bundle "install --without mysql --gemfile=./db/Gemfile --path=#{install_dir}/embedded/service/gem"
  command "#{install_dir}/embedded/bin/rsync -a --delete --exclude=.git/*** --exclude=.gitignore ./db/ #{install_dir}/embedded/service/opscode-reporting/db/"
end
