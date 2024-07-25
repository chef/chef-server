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

keydb = node['private_chef']['keydb_lb']
keydb_dir = keydb['dir']
keydb_etc_dir = File.join(keydb_dir, 'etc')
keydb_data_dir = keydb['data_dir']
keydb_data_dir_symlink = File.join(keydb_dir, 'data')
keydb_log_dir = keydb['log_directory']

[
  keydb_dir,
  keydb_etc_dir,
  keydb_data_dir,
  keydb_log_dir,
].each do |dir_name|
  directory dir_name do
    owner OmnibusHelper.new(node).ownership['owner']
    group OmnibusHelper.new(node).ownership['group']
    mode node['private_chef']['service_dir_perms']
    recursive true
  end
end

keydb_config = File.join(keydb_etc_dir, 'keydb.conf')

link keydb_data_dir_symlink do
  to keydb_data_dir
  not_if { keydb_data_dir_symlink == keydb_data_dir }
end

keydb_data = keydb

# == keydb service and config ==
# Because we need a running keydb service into which we can load
# XDarkLaunch presets, we need to be very careful about how we
# generate configs and trigger restarts. This is the procedure:
#
# 1. Write the Keydb config first.
#    This allows keydb to come up cleanly on first boot. In
#    previous versions of this cookbook, we wrote the config
#    after defining the keydb_lb service, causing the service
#    to fail on initial boot and requiring an immediate restart
#    of the service once the config was generated. By generating
#    the config beforehand, we can avoid this scenario.
#
# 2. Define the keydb_lb runit service.
#
# 3. Restart the keydb_lb runit service.
#    This step takes the place of the config notifying the service
#    to restart. Since we _ALWAYS_ need the keydb_lb service
#    running during a reconfigure, we shold always ensure that the
#    service is running right before we set the XDarkLaunch
#    presets. Restart will start services that are stopped.

# Write the Keydb config
template keydb_config do
  source 'keydb_lb.conf.erb'
  owner 'root'
  group 'root'
  mode '0644'
  variables(keydb_data.to_hash)
end

# Define the keydb_lb runit service.
component_runit_service 'keydb_lb'

# Restart the keydb_lb runit service.
runit_service 'keydb_lb' do
  action :restart
  retries 10
  retry_delay 1
  only_if { is_data_master? }
end

# log rotation
template "/etc/#{ChefUtils::Dist::Org::LEGACY_CONF_DIR}/logrotate.d/keydb_lb" do
  source 'logrotate.erb'
  owner 'root'
  group 'root'
  mode '0644'
  variables(keydb.to_hash.merge(
    'owner' => OmnibusHelper.new(node).ownership['owner'],
    'group' => OmnibusHelper.new(node).ownership['group']
  ))
end

#
# This should be guarded by a test that keydb is running.
#
# For the time being we retry a few times. This avoids a race
# condition where the server is still starting and the port isn't
# bound. The keydb gem does not retry on ECONNREFUSED, and we fail.
#
ruby_block 'set_lb_keydb_values' do
  retries 5
  retry_delay 1
  only_if { is_data_master? }
  block do
    require 'redis'
    keydb = Redis.new(host: keydb_data['vip'],
                      port: keydb_data['port'],
                      username: 'default',
                      password: PrivateChef.credentials.get('keydb_lb', 'password'))
    xdl = node['private_chef']['lb']['xdl_defaults']
    xmaint_allowed_ips_list = node['private_chef']['lb']['xmaint_allowed_ips_list']
    banned_ips = PrivateChef['banned_ips']
    maint_mode_ips = PrivateChef['maint_mode_whitelist_ips']
    # Ensure there is no stale data, but first institute
    # a brief maint mode to avoid potential misrouting when
    # we delete old keys.
    keydb.hset 'dl_default', '503_mode', true
    next until keydb.spop('banned_ips').nil?
    next until keydb.spop('xmaint_allowed_ips_list').nil?
    keys = keydb.hkeys 'dl_default'

    # Clear all dl_default keys except for the 503 mode we just set.
    keydb.pipelined do
      keys.each do |key|
        keydb.hdel 'dl_default', key unless key == '503_mode'
      end
    end

    keydb.pipelined do
      # Now we're clear to repopulate from configuration.
      unless banned_ips.nil?
        banned_ips.each do |ip|
          keydb.sadd 'banned_ips', ip
        end
      end
      xmaint_allowed_ips_list&.each do |ip|
        keydb.sadd 'xmaint_allowed_ips_list', ip
      end
      # Note that we'll preserve 503 mode until everything is
      # populated.
      unless xdl.nil?
        xdl.each do |key, value|
          keydb.hset('dl_default', key, value) unless key == '503_mode'
        end
      end
    end

    if xdl && xdl.key?('503_mode')
      keydb.hset 'dl_default', '503_mode', xdl['503_mode']
    else
      keydb.hdel 'dl_default', '503_mode'
    end
  end
  action :run
end
