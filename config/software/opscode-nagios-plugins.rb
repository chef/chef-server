name "opscode-nagios-plugins"

dependencies ["rsync"]

version "master"

source :git => "git@github.com:opscode/opscode-nagios-plugins"

relative_path "opscode-nagios-plugins"

build do
  command "mkdir -p /opt/opscode/embedded/nagios/libexec"
  command "/opt/opscode/embedded/bin/rsync -a ./plugins/ /opt/opscode/embedded/nagios/libexec/"
end
