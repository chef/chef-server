#
# Copyright:: Copyright (c) 2015 Chef Software, Inc.
#
# All Rights Reserved
#

# Only "enabled false" on yum repos, because
# the default contents of "/etc/apt/apt.conf.d/50unattended-upgrades"
# on an Ubuntu system would prevent chef-stable packages from being auto upgraded.
case node['platform_family']
when 'debian'

  package "apt-transport-https"

  apt_repository 'chef-stable' do
    uri "https://packagecloud.io/chef/stable/ubuntu/"
    key 'https://packagecloud.io/gpg.key'
    distribution node['private_chef']['addons']['ubuntu_distribution']
    deb_src true
    trusted true
    components %w( main )
  end

  # Performs an apt-get update
  include_recipe 'apt::default'

when 'rhel'

  major_version = if node['platform'] == 'amazon' then
                    '7' # Hard-code to EL7 for Amazon Linux.  the platform_version on AL
                        # looks like '2015.09' which doesn't then make a valid yum repo path
                    else
                      node['platform_version'].split('.').first
                    end

  gpg_key_path = File.join(node['private_chef']['install_path'], "/embedded/keys/packages-chef-io-public.key")

  yum_repository 'chef-stable' do
    description 'Chef Stable Repo'
    baseurl "https://packagecloud.io/chef/stable/el/#{major_version}/$basearch"
    gpgkey "file://#{gpg_key_path}"
    sslverify true
    sslcacert '/etc/pki/tls/certs/ca-bundle.crt'
    gpgcheck true
    enabled false
    action :create
  end

else
  # TODO: probably don't actually want to fail out?  Say, on any platform where
  # this would have to be done manually.
  raise "I don't know how to install addons for platform family: #{node['platform_family']}"
end
