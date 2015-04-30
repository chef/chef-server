#
# Author:: Douglas Triggs (<doug@getchef.com>)
# Copyright:: Copyright (c) 2014 Chef Software, Inc.
#
# All Rights Reserved
#

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

  major_version = node['platform_version'].split('.').first

  gpg_key_path = File.join(node['private_chef']['install_path'], "/embedded/keys/packages-chef-io-public.key")

  yum_repository 'chef-stable' do
    description 'Chef Stable Repo'
    baseurl "https://packagecloud.io/chef/stable/el/#{major_version}/$basearch"
    gpgkey "file://#{gpg_key_path}"
    sslverify true
    sslcacert '/etc/pki/tls/certs/ca-bundle.crt'
    gpgcheck true
    action :create
  end

else
  # TODO: probably don't actually want to fail out?  Say, on any platform where
  # this would have to be done manually.
  raise "I don't know how to install addons for platform family: #{node['platform_family']}"
end

node['private_chef']['addons']['packages'].each do |pkg|
  package pkg do
    notifies :create, "ruby_block[addon_install_notification_#{pkg}]", :immediate
  end
end
