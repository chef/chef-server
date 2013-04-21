name "opscode-nagios-plugins"

dependency "rsync"

version "pc-rel-1.0.0"

source :git => "git@github.com:opscode/opscode-nagios-plugins"

relative_path "opscode-nagios-plugins"

build do
  command "mkdir -p #{install_dir}/embedded/nagios/libexec"
  command "sudo #{install_dir}/embedded/bin/rsync -a ./plugins/ #{install_dir}/embedded/nagios/libexec/"

  # check_redis needs the redis gem installed to the main gem repo
  gem "install redis -v 2.2.2"
end
