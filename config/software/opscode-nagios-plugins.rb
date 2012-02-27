name "opscode-nagios-plugins"

dependencies ["rsync"]

version "master"

source :git => "git@github.com:opscode/opscode-nagios-plugins"

relative_path "opscode-nagios-plugins"

build do
  command "mkdir -p #{install_dir}/embedded/nagios/libexec"
  command "sudo #{install_dir}/embedded/bin/rsync -a ./plugins/ #{install_dir}/embedded/nagios/libexec/"
end
