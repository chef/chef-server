#
# Author:: Douglas Triggs (<doug@getchef.com>)
# Copyright:: Copyright (c) 2014 Chef Software, Inc.
#
# All Rights Reserved
#

case node['platform_family']
when 'debian'

  apt_repository 'chef-stable' do
    uri "https://packagecloud.io/chef/test-stable/ubuntu/"
    key 'https://packagecloud.io/gpg.key'
    distribution node['lsb']['codename']
    deb_src true
    trusted true
    components %w( main )
  end

  # Performs an apt-get update
  include_recipe 'apt::default'

when 'rhel'

  major_version = node['platform_version'].split('.').first

  yum_repository 'chef-stable' do
    description 'Chef Stable Repo'
    baseurl "https://packagecloud.io/chef/test-stable/el/#{major_version}/$basearch"
    gpgkey 'https://packagecloud.io/gpg.key'
    sslverify true
    sslcacert '/etc/pki/tls/certs/ca-bundle.crt'
    gpgcheck false
    action :create
  end

else
  # TODO: probably don't actually want to fail out?  Say, on any platform where
  # this would have to be done manually.
  raise "I don't know how to install addons for platform family: #{node['platform_family']}"
end

node['private_chef']['addons']['packages'].each do |pkg|
  package pkg
end
