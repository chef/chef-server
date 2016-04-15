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
  ret = {}
  node['private_chef']['chef_backend_members'].each do |member|
    begin
      client = Chef::HTTP.new("http://#{member}:#{node['private_chef']['haproxy']['etcd_port']}")
      members = JSON.parse(client.get("/v2/members"))["members"]
      if members && !members.empty?
        break
      else
        next
      end
    rescue
      next
    end
  end
  if members
    members.each do |m|
      ret[m["name"]] = URI.parse(m["peerURLs"].first).host
    end
    ret
  else
    nil
  end
end

def configured_members
  ret = {}
  node['private_chef']['chef_backend_members'].each_with_index do |member, i|
    ret["#{backend}#{i}"] = member
  end
  ret
end

chef_backend_members = begin
                         Chef::Log.info("Attempting Chef Backend Member Discovery")
                         if members = get_chef_backend_cluster_members
                           Chef::Log.info("Using Chef Backend members discovered via etcd")
                           members
                         else
                           Chef::Log.info("Member discovery failed")
                           Chef::Log.info("Using statically configured member list")
                           configured_members
                         end
                       rescue e
                         Chef::Log.info("member discoverry failed: #{e}")
                         Chef::Log.info("Using statically configured member list")
                         configured_members
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
      if ::File.exist?("/var/opt/opscode/haproxy/haproxy.sock")
        require 'socket'
        begin
          UNIXSocket.new("/var/opt/opscode/haproxy/haproxy.sock")
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
end

ruby_block "wait for backend leader to stabilize" do
  block do
    stable = false
    10.times do
      require 'socket'
      begin
        s = UNIXSocket.new("/var/opt/opscode/haproxy/haproxy.sock")
        s.puts "show stat;quit"
        _header = s.gets
        table = []
        up_servers = {
          "chef_backend_elasticsearch" => [],
          "chef_backend_postgresql" => [],
        }
        while line = s.gets
          table << line
        end

        table.each do |l|
          # show stat returns a csv formatted output. For now we do
          # the parsing ourselves since we have simple needs.
          split_line = l.split(",")
          pxname = split_line[0] # Name of the service (ex: chef_backend_elasticsearch)
          svname = split_line[1] # Name of this server or "BACKEND" or "FRONTEND"
          state = split_line[17] # UP/DOWN/MAINT

          # Ignore lines of the table not related to backend server entries
          if svname != "BACKEND" && state == "UP" &&
             ["chef_backend_elasticsearch", "chef_backend_postgresql"].include?(pxname)
            up_servers[pxname] << svname
          end
        end

        # We expect the status checks to fail on all but 1 backend
        # (the current leader) thus we wait for that to be the case.
        if up_servers["chef_backend_elasticsearch"].count == 1 &&
           up_servers["chef_backend_postgresql"].count == 1
          stable = true
          break
        else
          Chef::Log.warn("HAProxy still inconsistent:")
          Chef::Log.warn("  Postgresql servers UP:")
          up_servers["chef_backend_postgresql"].each do |server_name|
            Chef::Log.warn("   -#{server_name}")
          end
          Chef::Log.warn("  Elasticsearch servers UP:")
          up_servers["chef_backend_elasticsearch"].each do |server_name|
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
