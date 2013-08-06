name "chef-sql-schema"
version "pc-rel-1.13.0"

dependency "ruby"
dependency "bundler"
dependency "postgresql92"
dependency "rsync"

source :git => "git@github.com:opscode/chef-sql-schema.git"

relative_path "chef-sql-schema"

build do
  bundle "install --without mysql --path=/opt/opscode/embedded/service/gem"
  command "mkdir -p #{install_dir}/embedded/service/chef-sql-schema"
  command "#{install_dir}/embedded/bin/rsync -a --delete --exclude=.git/*** --exclude=.gitignore ./ #{install_dir}/embedded/service/chef-sql-schema/"
end
