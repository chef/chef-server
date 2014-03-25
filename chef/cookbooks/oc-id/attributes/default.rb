#
# Cookbook Name:: oc-id
# Attributes:: default
# Author: Chris Nunciato <cnunciato@getchef.com>
# All rights reserved - Do Not Redistribute
#

node.default['oc-id']['install_dir'] = '/srv/oc-id'
node.default['oc-id']['rails_env'] = 'production'
node.default['oc-id']['group'] = 'opscode'
node.default['oc-id']['user'] = 'opscode'
node.default['oc-id']['port'] = 4060
