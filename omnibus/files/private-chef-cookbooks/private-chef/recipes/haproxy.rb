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
