# Copyright:: 2012-2018 Chef Software, Inc.
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
redis_service = "/opt/opscode/service/redis_lb"
runit_sv_path = "/opt/opscode/embedded/bin/sv"

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
ruby_block 'restarting redis_lb' do
  block do
    started = false
    10.times do
        command = Mixlib::ShellOut.new("#{runit_sv_path} restart #{redis_service}")
        command.run_command
        if !command.status.success? 
          sleep 1
        else
          started = true
          break
        end
      end

      unless started
        Chef::Log.fatal('Failed to restart redis_lb runit service')
        Kernel.exit! 1
      end
  end
  notifies :restart, 'component_runit_service[redis_lb]'
end

# log rotation
template '/etc/opscode/logrotate.d/redis_lb' do
  source 'logrotate.erb'
  owner 'root'
  group 'root'
  mode '0644'
  variables(redis.to_hash)
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
    redis = Redis.new(host: redis_data['vip'],
                      port: redis_data['port'],
                      password: PrivateChef.credentials.get('redis_lb', 'password'))
    xdl = node['private_chef']['lb']['xdl_defaults']
    banned_ips = PrivateChef['banned_ips']
    maint_mode_ips = PrivateChef['maint_mode_whitelist_ips']
    # Ensure there is no stale data, but first institute
    # a brief maint mode to avoid potential misrouting when
    # we delete old keys.
    redis.hset 'dl_default', '503_mode', true
    next until redis.spop('banned_ips').nil?
    next until redis.spop('maint_data').nil?
    keys = redis.hkeys 'dl_default'

    # Clear all dl_default keys except for the 503 mode we just set.
    redis.pipelined do
      keys.each do |key|
        redis.hdel 'dl_default', key unless key == '503_mode'
      end
    end

    redis.pipelined do
      # Now we're clear to repopulate from configuration.
      unless banned_ips.nil?
        banned_ips.each do |ip|
          redis.sadd   'banned_ips', ip
        end
      end
      unless maint_mode_ips.nil?
        maint_mode_ips.each do |ip|
          redis.sadd   'maint_data', ip
        end
      end
      # Note that we'll preserve 503 mode until everything is
      # populated.
      unless xdl.nil?
        xdl.each do |key, value|
          redis.hset('dl_default', key, value) unless key == '503_mode'
        end
      end
    end

    if xdl && xdl.key?('503_mode')
      redis.hset 'dl_default', '503_mode', xdl['503_mode']
    else
      redis.hdel 'dl_default', '503_mode'
    end
  end
  action :run
end
