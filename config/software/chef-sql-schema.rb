name "chef-sql-schema"
version "pc-rel-1.15.1"

dependency "ruby"
dependency "bundler"
dependency "postgresql92"
dependency "rsync"

source :git => "git@github.com:opscode/chef-sql-schema.git"

relative_path "chef-sql-schema"

# Since this project pulls in the pg gem (or depends on something that
# does) we need to have the pg_config binary on the PATH so the
# correct library and header locations can be found
env = {
  'PATH' => "#{install_dir}/embedded/bin:#{ENV['PATH']}"
}

build do
  bundle "install --path=/opt/opscode/embedded/service/gem", :env => env
  command "mkdir -p #{install_dir}/embedded/service/chef-sql-schema"
  command "#{install_dir}/embedded/bin/rsync -a --delete --exclude=.git/*** --exclude=.gitignore ./ #{install_dir}/embedded/service/chef-sql-schema/"
end
