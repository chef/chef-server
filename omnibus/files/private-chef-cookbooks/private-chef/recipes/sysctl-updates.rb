
# Author:: Marc Paradise (<marc@chef.io>)
# Copyright:: 2013-2018 Chef Software, Inc.
#
# All Rights Reserved


# Typically speaking supported systems will default to this value
# but we'll want to ensure that it's correct. Many of our services currently rely on
# automatic dual-binding of listening ports to both ipv6 and ipv4.  This directive ensures
# the behavior.
# net.ipv6.bindv6only = 0


execute "sysctl-reload" do
  command "/sbin/sysctl -p /etc/sysctl.conf || true"
  action :nothing
end

# Ideally, we'd just render a template into sysctl.d - but that's
# 6.2 and later under centos. This method will work for all supported
# platforms.

bash "dual ip4/ip6 portbind" do
  user "root"
  code <<-EOF
    echo 'net.ipv6.bindv6only = 0' >> /etc/sysctl.conf
  EOF
  notifies :run, 'execute[sysctl-reload]', :immediately
  not_if "egrep '^net\.ipv6\.bindv6only = 0' /etc/sysctl.conf"
  only_if { PrivateChef["use_ipv6"] == true }
end
