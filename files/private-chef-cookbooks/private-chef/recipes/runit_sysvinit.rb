#
# Copyright:: Copyright (c) 2013 Opscode, Inc.
#
# All Rights Reserved
#

# We assume you are sysvinit
svdir_line = 'SV:123456:respawn:/opt/opscode/embedded/bin/runsvdir-start'
execute "echo '#{svdir_line}' >> /etc/inittab" do
  not_if "grep '#{svdir_line}' /etc/inittab"
  notifies :run, "execute[init q]", :immediately
end

execute "init q" do
  action :nothing
end
