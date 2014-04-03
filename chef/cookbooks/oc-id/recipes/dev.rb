#
# Cookbook Name:: oc-id
# Recipe:: dev
# Author:: Chris Nunciato <cnunciato@getchef.com>
# All rights reserved - Do Not Redistribute
#

node_attrs = node['oc-id']
user_name = node_attrs['user']
group_name = node_attrs['group']
rails_env = node_attrs['rails_env'] || 'production'
install_dir = node_attrs['install_dir']

file '/etc/opscode/webui_priv.pem' do
  user user_name
  group group_name
  content IO.read('/tmp/oc-id/config/webui_priv.pem')
  notifies :restart, 'service[oc-id]'
end

template "#{install_dir}/current/config/settings/#{rails_env}.yml" do
  user user_name
  group group_name
  source 'settings.yml.erb'
  mode 0644
  variables node_attrs.merge({
    :hostnames => {
      'lb_int_servername' => '33.33.33.10'
    }
  }).to_hash
end
