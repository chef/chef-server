# Copyright:: Chef Software, Inc.
# Author:: Marc A. Paradise <marc@chef.io>
# License:: Apache License, Version 2.0
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

redis = node['private_chef']['redis_lb']
redis_dir = redis['dir']
redis_etc_dir = File.join(redis_dir, 'etc')
redis_data_dir = redis['data_dir']
redis_data_dir_symlink = File.join(redis_dir, 'data')
redis_log_dir = redis['log_directory']

[
  redis_dir,
  redis_etc_dir,
  redis_data_dir,
  redis_log_dir,
].each do |dir_name|
  directory dir_name do
    owner OmnibusHelper.new(node).ownership['owner']
    group OmnibusHelper.new(node).ownership['group']
    mode node['private_chef']['service_dir_perms']
    recursive true
  end
end

redis_config = File.join(redis_etc_dir, 'redis.conf')

link redis_data_dir_symlink do
  to redis_data_dir
  not_if { redis_data_dir_symlink == redis_data_dir }
end

redis_data = redis

# == redis service and config ==
# Because we need a running redis service into which we can load
# XDarkLaunch presets, we need to be very careful about how we
# generate configs and trigger restarts. This is the procedure:
#
# 1. Write the Redis config first.
#    This allows redis to come up cleanly on first boot. In
#    previous versions of this cookbook, we wrote the config
#    after defining the redis_lb service, causing the service
#    to fail on initial boot and requiring an immediate restart
#    of the service once the config was generated. By generating
#    the config beforehand, we can avoid this scenario.
#
# 2. Define the redis_lb runit service.
#
# 3. Restart the redis_lb runit service.
#    This step takes the place of the config notifying the service
#    to restart. Since we _ALWAYS_ need the redis_lb service
#    running during a reconfigure, we shold always ensure that the
#    service is running right before we set the XDarkLaunch
#    presets. Restart will start services that are stopped.

# Write the Redis config
template redis_config do
  source 'redis_lb.conf.erb'
  owner 'root'
  group 'root'
  mode '0644'
  variables(redis_data.to_hash)
end

# Define the redis_lb runit service.
component_runit_service 'redis_lb'

# Restart the redis_lb runit service.
runit_service 'redis_lb' do
  action :restart
  retries 10
  retry_delay 1
  only_if { is_data_master? }
end

# log rotation
template "/etc/#{ChefUtils::Dist::Org::LEGACY_CONF_DIR}/logrotate.d/redis_lb" do
  source 'logrotate.erb'
  owner 'root'
  group 'root'
  mode '0644'
  variables(redis.to_hash.merge(
    'owner' => OmnibusHelper.new(node).ownership['owner'],
    'group' => OmnibusHelper.new(node).ownership['group']
  ))
end

#
# This should be guarded by a test that redis is running.
#
# For the time being we retry a few times. This avoids a race
# condition where the server is still starting and the port isn't
# bound. The redis gem does not retry on ECONNREFUSED, and we fail.
#
ruby_block 'set_lb_redis_values' do
  retries 5
  retry_delay 1
  only_if { is_data_master? }
  block do
    require 'redis'
    redis = Redis.new(host: redis_data['vip'].to_s,
                      port: redis_data['port'].to_i,
                      username: 'default',
                      password: PrivateChef.credentials.get('redis_lb', 'password').to_s)
    
    # Convert all values to strings to avoid type issues
    xdl = node['private_chef']['lb']['xdl_defaults'].map { |k, v| [k, v.to_s] }.to_h
    xmaint_allowed_ips_list = node['private_chef']['lb']['xmaint_allowed_ips_list'].map(&:to_s)
    
    redis.set('xdl_defaults', xdl.to_json)
    redis.set('xmaint_allowed_ips_list', xmaint_allowed_ips_list.to_json)
  end
end
