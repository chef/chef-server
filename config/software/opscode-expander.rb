name "opscode-expander"
version "pc-rel-1.0.0.1"

dependency "ruby"
dependency "bundler"
dependency "rsync"

source :git => "git@github.com:opscode/opscode-expander"

relative_path "opscode-expander"

build do
  bundle "install --without mysql --path=/opt/opscode/embedded/service/gem"
  command "mkdir -p #{install_dir}/embedded/service/opscode-expander"
  command "#{install_dir}/embedded/bin/rsync -a --delete --exclude=.git/*** --exclude=.gitignore ./ #{install_dir}/embedded/service/opscode-expander/"
end
