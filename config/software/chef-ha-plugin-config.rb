name "chef-ha-plugin-config"
description "generates chef-server-plugin.rb"
default_version "0.0.1"

build do
  block do
    File.open("#{install_dir}/chef-server-plugin.rb", "w") do |f|
      f.puts <<EOF
plugin "chef-ha-drbd" do
  enabled_by_default false
end
EOF
    end
  end
end
