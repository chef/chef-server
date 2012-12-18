name "private-chef-ctl"

dependencies [ "rsync", "omnibus-ctl" ]

source :path => File.expand_path("files/private-chef-ctl-commands", Omnibus.root)

build do
  block do
    open("#{install_dir}/bin/private-chef-ctl", "w") do |file|
      file.print <<-EOH
#!/bin/bash
#
# Copyright:: Copyright (c) 2012 Opscode, Inc.
#
# All Rights Reserved
#

#{install_dir}/embedded/bin/omnibus-ctl opscode #{install_dir}/embedded/service/omnibus-ctl $@
       EOH
    end
  end

  command "chmod 755 #{install_dir}/bin/private-chef-ctl"

  # additional omnibus-ctl commands
  command "#{install_dir}/embedded/bin/rsync -a ./ #{install_dir}/embedded/service/omnibus-ctl/"
end
