# Copyright:: Copyright (c) 2012 Opscode, Inc.
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
haproxy_dir = node['private_chef']['haproxy']['dir']
haproxy_socket = ::File.join(haproxy_dir, "haproxy.sock")
haproxy_log_dir = node['private_chef']['haproxy']['log_directory']

[
  haproxy_dir,
  haproxy_log_dir
].each do |dir_name|
  directory dir_name do
    owner OmnibusHelper.new(node).ownership['owner']
    group OmnibusHelper.new(node).ownership['group']
    mode node['private_chef']['service_dir_perms']
    recursive true
  end
end

#
# We need to configure haproxy with the members in our Chef Backend
# cluster. The user should have provided an array of chef-backend
# members. We iterate through this array and try to contact the member
# on etcd to get the most up-to-date list. If any part of that fails,
# we fall-back to the user-configured list of hosts.
#
def get_chef_backend_cluster_members
  members = nil
  node['private_chef']['chef_backend_members'].each do |member|
    begin
      members = ChefBackend.etcd_members(member, node['private_chef']['haproxy']['etcd_port'])
      break if members && !members.empty?
    rescue StandardError => e
      Chef::Log.warn("Error attempting to get cluster members from #{member}:")
      Chef::Log.warn("  #{e}")
      Chef::Log.warn("Trying next configured chef_backend member.")
    end
  end
  members
end

chef_backend_members = begin
                         Chef::Log.info("Attempting Chef Backend Member Discovery")
                         if members = get_chef_backend_cluster_members
                           Chef::Log.info("Using Chef Backend members discovered via etcd")
                           members
                         else
                           Chef::Log.warn("Member discovery failed")
                           Chef::Log.warn("Using statically configured member list")
                           ChefBackend.configured_members(node)
                         end
                       rescue StandardError => e
                         Chef::Log.warn("member discoverry failed: #{e}")
                         Chef::Log.warn("Using statically configured member list")
                         ChefBackend.configured_members(node)
                       end

template File.join(haproxy_dir, "haproxy.cfg") do
  source "haproxy.cfg.erb"
  owner OmnibusHelper.new(node).ownership['owner']
  group OmnibusHelper.new(node).ownership['group']
  mode "600"
  variables(node['private_chef']['haproxy'].to_hash.merge(chef_backend_members: chef_backend_members))
  notifies :restart, 'runit_service[haproxy]'
end

component_runit_service "haproxy"

# On startup, all backend servers will be marked as UP.
#
# As the heathcheck on the non-leaders fails, they will be marked as
# down. This usually takes 2-3 seconds, which is long enough for the
# reconfigure to proceed to attempting to bootstrap erchef, which
# won't work if we end up talking to a follower in read-only mode.
#
# Here, we wait for the HAProxy stats output to confirm that only one
# server in each backend group (the leader) is marked as up to avoid
# trying to bootstrap against the read-only follower.
ruby_block "wait for haproxy status socket" do
  block do
    connected = false
    10.times do
      if ::File.exist?(haproxy_socket)
        require 'socket'
        begin
          UNIXSocket.new(haproxy_socket)
          connected = true
          break
        rescue
          sleep 1
        end
      else
        sleep 1
      end
    end

    if !connected
      Chef::Log.fatal("HAProxy status socket never appeared properly!")
      Chef::Log.fatal("See /var/log/opscode/haproxy/current for more information")
      Kernel.exit! 1
    end
  end
  notifies :start, 'runit_service[haproxy]', :before
end

ruby_block "wait for backend leader to stabilize" do
  block do
    stable = false
    10.times do
      begin
        require 'socket'
        s = HAProxyStatus.new(UNIXSocket.new(haproxy_socket))
        active_servers = {
          "chef_backend_elasticsearch" => [],
          "chef_backend_postgresql" => []
        }

        s.server_stats.each do |server|
          active_servers[server[:pxname]] << server[:svname] if server[:status] == "UP"
        end

        # We expect the status checks to fail on all but 1 backend
        # (the current leader) thus we wait for that to be the case.
        if active_servers["chef_backend_elasticsearch"].count == 1 &&
           active_servers["chef_backend_postgresql"].count == 1
          stable = true
          break
        else
          Chef::Log.warn("HAProxy still inconsistent:")
          Chef::Log.warn("  Postgresql servers UP:")
          active_servers["chef_backend_postgresql"].each do |server_name|
            Chef::Log.warn("   -#{server_name}")
          end
          Chef::Log.warn("  Elasticsearch servers UP:")
          active_servers["chef_backend_elasticsearch"].each do |server_name|
            Chef::Log.warn("   -#{server_name}")
          end
          Chef::Log.warn("Retrying in 2 seconds")
          sleep 2
        end
      rescue StandardError => e
        Chef::Log.warn("Error attempting to verify HAProxy State:")
        Chef::Log.warn("   #{e}")
        Chef::Log.warn("Retrying in 2 seconds")
        sleep 2
      end
    end

    if !stable
      Chef::Log.fatal("HAProxy still showing multiple active backends")
      Chef::Log.fatal("Please check /var/log/opscode/haproxy/current locally for problems.")
      Chef::Log.fatal("Please check your backend cluster's status for problems.")
      Kernel.exit! 1
    end
  end
end
