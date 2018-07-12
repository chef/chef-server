#
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

# This action should only be called during HA upgrades on the bootstrap
# Explanation: During the PC1.4 -> EC11 upgrade, the opscode-runsvdir upstart
# service is stopped and removed (replaced by private-chef-runsvdir).  This
# causes a restart of keepalived into a bad state until the end of the reconfigure.
# The goal of this LWRP is to put keepalived into a good state first, so we
# don't experience an unexpected cluster state transition during reconfigure.
action :create do

  if ha? && is_data_master?

    keepalived_dir = node['private_chef']['keepalived']['dir']
    keepalived_etc_dir = ::File.join(keepalived_dir, "etc")
    keepalived_log_dir = node['private_chef']['keepalived']['log_directory']

    # Needed because of OC-11490
    directory keepalived_log_dir do
      owner node['private_chef']['user']['username']
      recursive true
      mode "0700"
    end

    # override default keepalived state to MASTER so we don't stop
    keepalived_options = node['private_chef']['keepalived'].to_hash
    keepalived_options['vrrp_instance_state'] = 'MASTER'

    template ::File.join(keepalived_etc_dir, "keepalived.conf") do
      source "keepalived.conf.erb"
      owner "root"
      group "root"
      mode "0644"
      variables(keepalived_options)
    end

    # rewrite the sv/keepalived/run file to have the correct flags before a restart
    # NOTE: keepalived restart happens here, but it *should* not transition to backup
    component_runit_service "keepalived"

    # The previous resource will bounce keepalived, causing a transition from
    # master -> backup.  It will then attempt to transition back to master but
    # cannot succeed until reconfigure has finished.
    # The goal here is to wait until 'requested_cluster_status' has changed back
    # to 'master', meaning keepalived wants to be master, and it has at least
    # successfully mounted the DRBD volume.
    ruby_block 'wait_for_drbd_mount' do
      block do
        requested_cluster_status_file = ::File.join(node['private_chef']['keepalived']['dir'],
          'requested_cluster_status')

        puts 'keepalived restarted, waiting for DRBD mount to return'
        STDOUT.sync = true
        (0..120).each do |attempt|
          break if
            ::File.read('/proc/mounts').include?(node['private_chef']['drbd']['data_dir']) &&
            ::File.exists?(requested_cluster_status_file) &&
            ::File.open(requested_cluster_status_file).read.chomp == 'master'

          if attempt == 120
              raise 'ERROR: Timeout waiting for DRBD mount to return'
          end
          print '.'
          sleep 1
        end
        puts 'DRBD mount has returned, proceeding'
      end
    end

  end

end
