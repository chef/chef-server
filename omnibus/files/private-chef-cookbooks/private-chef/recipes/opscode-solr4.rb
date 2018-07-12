# -*- coding: utf-8 -*-
#
# Author:: Adam Jacob (<adam@chef.io>)
# Copyright:: 2011-2018 Chef Software, Inc.
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

solr_dir              = node['private_chef']['opscode-solr4']['dir']            # /var/opt/opscode/opscode-solr4
solr_data_dir         = node['private_chef']['opscode-solr4']['data_dir']       # /var/opt/opscpde/opscode-solr4/data
solr_data_dir_symlink = File.join(solr_dir, "data")                             # /var/opt/opscode/opscode-solr4/data
solr_home_dir         = File.join(solr_dir, "home")                             # /var/opt/opscode/opscode-solr4/home
solr_temp_dir         = node['private_chef']['opscode-solr4']['temp_directory'] # /var/opt/opscode/opscode-solr4
solr_log_dir          = node['private_chef']['opscode-solr4']['log_directory']  # /var/log/opscode/opscode-solr4
solr_collection_dir   = File.join(solr_home_dir, "collection1")                 # /var/opt/opscode/opscode-solr4/home/collection1
solr_conf_dir         = File.join(solr_collection_dir, "conf")                  # /var/opt/opscode/opscode-solr4/home/collection1/conf
solr_jetty_dir        = "/opt/opscode/embedded/service/opscode-solr4/jetty"


# set up the basic solr directory structure
[ solr_dir,
  solr_data_dir,
  solr_home_dir,
  solr_temp_dir,
  solr_log_dir,
  solr_collection_dir,
  solr_conf_dir
].each do |dir_name|
  directory dir_name do
    owner OmnibusHelper.new(node).ownership['owner']
    group OmnibusHelper.new(node).ownership['group']
    mode node['private_chef']['service_dir_perms']
    recursive true
  end
end

# convenience symlink for finding solr data (if not in solr dir)
link solr_data_dir_symlink do
  to solr_data_dir
  not_if { solr_data_dir == solr_data_dir_symlink }
end

## Solr 4 Home Structure
# .
# +-- collection1
# |   +-- conf
# |   |   +-- schema.xml
# |   |   +-- solrconfig.xml
# |   +-- core.properties
# +-- solr.xml

cookbook_file File.join(solr_home_dir, "solr.xml") do
  source "solr4/solr.xml"
  owner OmnibusHelper.new(node).ownership['owner']
  group OmnibusHelper.new(node).ownership['group']
  mode "0644"
  notifies :restart, 'runit_service[opscode-solr4]' if is_data_master?
end

file File.join(solr_collection_dir, "core.properties") do
  owner OmnibusHelper.new(node).ownership['owner']
  group OmnibusHelper.new(node).ownership['group']
  mode "0644"
  notifies :restart, 'runit_service[opscode-solr4]' if is_data_master?
  content <<EOF
name=collection1
EOF
end

template File.join(solr_conf_dir, "solrconfig.xml") do
  source "solr4/solrconfig.xml.erb"
  owner OmnibusHelper.new(node).ownership['owner']
  group OmnibusHelper.new(node).ownership['group']
  mode "0644"
  variables(node['private_chef']['opscode-solr4'].to_hash)
  notifies :restart, 'runit_service[opscode-solr4]' if is_data_master?
end

cookbook_file File.join(solr_conf_dir, "schema.xml") do
  source "solr4/schema.xml"
  owner OmnibusHelper.new(node).ownership['owner']
  group OmnibusHelper.new(node).ownership['group']
  mode "0644"
  notifies :restart, 'runit_service[opscode-solr4]' if is_data_master?
end

template File.join(solr_jetty_dir, "etc", "jetty.xml") do
  owner OmnibusHelper.new(node).ownership['owner']
  group OmnibusHelper.new(node).ownership['group']
  mode "0644"
  source "solr4/jetty.xml.erb"
  variables(node['private_chef']['opscode-solr4'].to_hash.merge(node['private_chef']['logs'].to_hash))
  notifies :restart, 'runit_service[opscode-solr4]' if is_data_master?
end

template File.join(solr_jetty_dir, "contexts", "solr-jetty-context.xml") do
  owner OmnibusHelper.new(node).ownership['owner']
  group OmnibusHelper.new(node).ownership['group']
  mode "0644"
  source "solr4/solr-jetty-context.xml.erb"
  notifies :restart, 'runit_service[opscode-solr4]' if is_data_master?
end


execute "chown -R #{OmnibusHelper.new(node).ownership['owner']} #{solr_jetty_dir}"

node.default['private_chef']['opscode-solr4']['command'] =  "java -Xmx#{node['private_chef']['opscode-solr4']['heap_size']} -Xms#{node['private_chef']['opscode-solr4']['heap_size']}"
# Compute some sane JVM tunings. The user can still override these computed
# defaults using /etc/opscode/private-chef.rb
solr_mem = if node['private_chef']['opscode-solr4']['heap_size']
             OmnibusHelper.parse_mem_to_mb(node['private_chef']['opscode-solr4']['heap_size'])
           else
             node['memory']['total'] =~ /^(\d+)kB/
             memory_total_in_mb = $1.to_i / 1024
             # Total heap size for solr is the smaller of:
             #    25% of total system memory
             #    1024 MB
             [(memory_total_in_mb / 4), 1024].min
           end
new_size =  if node['private_chef']['opscode-solr4']['new_size']
              OmnibusHelper.parse_mem_to_mb(node['private_chef']['opscode-solr4']['new_size'])
            else
              [(solr_mem / 16), 32].max
            end

java_opts = node['private_chef']['opscode-solr4']['java_opts'].dup
java_opts << " -XX:NewSize=#{new_size}M" unless java_opts =~ /NewSize/
java_opts << " -XX:+UseConcMarkSweepGC" unless java_opts =~ /UseConcMarkSweepGC/
java_opts << " -XX:+UseParNewGC" unless java_opts =~ /UseParNewGC/

# Save the values back onto the node attributes
node.default['private_chef']['opscode-solr4']['java_opts'] = java_opts
node.default['private_chef']['opscode-solr4']['heap_size'] = solr_mem
node.default['private_chef']['opscode-solr4']['new_size']  = new_size

command = "java -Xmx#{solr_mem}M -Xms#{solr_mem}M"
command << " #{java_opts}" unless java_opts.empty?
# Enable GC Logging (very useful for debugging issues) to an separate file only works with Oracle JRE
if node['kernel']['machine'] == "x86_64"
  command << " -Xloggc:#{File.join(solr_log_dir, "gclog.log")}"
end

# Enable GC Logging (very useful for debugging issues)
if node['private_chef']['opscode-solr4']['log_gc']
  command << " -verbose:gc -XX:+PrintHeapAtGC -XX:+PrintGCDateStamps -XX:+PrintGCDetails -XX:+PrintGCApplicationStoppedTime -XX:+PrintGCApplicationConcurrentTime -XX:+PrintTenuringDistribution"
  # have java rotate the gclog.log (to avoid issues around copytruncate and sparse
  # files, see SPOOL-383)
  command << " -XX:+UseGCLogFileRotation -XX:NumberOfGCLogFiles=#{node['private_chef']['opscode-solr4']['log_rotation']['num_to_keep']} -XX:GCLogFileSize=#{node['private_chef']['opscode-solr4']['log_rotation']['file_maxbytes']}"
end

command << " -Dsolr.data.dir=#{solr_data_dir}"
command << " -Dsolr.solr.home=#{solr_home_dir}"
command << " -Djava.io.tmpdir=#{solr_temp_dir}"
command << " -server"
command << " -jar '#{solr_jetty_dir}/start.jar'"

node.default['private_chef']['opscode-solr4']['command'] = command

component_runit_service "opscode-solr4"

# log rotation is now handled by java
file "/etc/opscode/logrotate.d/opscode-solr4" do
  action :delete
end
