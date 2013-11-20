#
# Author:: Adam Jacob (<adam@opscode.com>)
# Copyright:: Copyright (c) 2011 Opscode, Inc.
#
# All Rights Reserved
#

solr_dir = node['private_chef']['opscode-solr']['dir']
solr_etc_dir = File.join(solr_dir, "etc")
solr_jetty_dir = File.join(solr_dir, "jetty")
solr_data_dir = node['private_chef']['opscode-solr']['data_dir']
solr_data_dir_symlink = File.join(solr_dir, "data")
solr_home_dir = File.join(solr_dir, "home")
solr_log_dir = node['private_chef']['opscode-solr']['log_directory']

[ solr_dir, solr_etc_dir, solr_data_dir, solr_home_dir, solr_log_dir ].each do |dir_name|
  directory dir_name do
    owner node['private_chef']['user']['username']
    mode '0700'
    recursive true
  end
end

link solr_data_dir_symlink do
  to solr_data_dir
  not_if { solr_data_dir == solr_data_dir_symlink }
end

solr_config = File.join(solr_etc_dir, "solr.rb")

template File.join(solr_etc_dir, "solr.rb") do
  source "solr.rb.erb"
  owner "root"
  group "root"
  mode "0644"
  variables(node['private_chef']['opscode-solr'].to_hash)
end

solr_installed_file = File.join(solr_dir, "installed")

execute "cp -R /opt/opscode/embedded/service/opscode-solr/home/conf #{File.join(solr_home_dir, 'conf')}" do
  not_if { File.exists?(solr_installed_file) }
  notifies(:restart, "runit_service[opscode-solr]") if is_data_master?
end

execute "cp -R /opt/opscode/embedded/service/opscode-solr/jetty #{File.dirname(solr_jetty_dir)}" do
  not_if { File.exists?(solr_installed_file) }
  notifies(:restart, "runit_service[opscode-solr]") if is_data_master?
end

execute "chown -R #{node['private_chef']['user']['username']} #{solr_dir}" do
  not_if { File.exists?(solr_installed_file) }
end

file solr_installed_file do
  owner "root"
  group "root"
  mode "0644"
  content "Delete me to force re-install solr - dangerous"
  action :create
end

template File.join(solr_jetty_dir, "etc", "jetty.xml") do
  owner node['private_chef']['user']['username']
  mode "0644"
  source "jetty.xml.erb"
  variables(node['private_chef']['opscode-solr'].to_hash.merge(node['private_chef']['logs'].to_hash))
  notifies :restart, 'runit_service[opscode-solr]' if is_data_master?
end

template File.join(solr_home_dir, "conf", "solrconfig.xml") do
  owner node['private_chef']['user']['username']
  mode "0644"
  source "solrconfig.xml.erb"
  variables(node['private_chef']['opscode-solr'].to_hash)
  notifies :restart, 'runit_service[opscode-solr]' if is_data_master?
end

node.default['private_chef']['opscode-solr']['command'] =  "java -Xmx#{node['private_chef']['opscode-solr']['heap_size']} -Xms#{node['private_chef']['opscode-solr']['heap_size']}"
# Compute some sane JVM tunings. The user can still override these computed
# defaults using /etc/opscode/private-chef.rb
solr_mem = if node['private_chef']['opscode-solr']['heap_size']
              node['private_chef']['opscode-solr']['heap_size']
           else
             node[:memory][:total] =~ /^(\d+)kB/
             memory_total_in_mb = $1.to_i / 1024
             # Total heap size for solr is the smaller of:
             #    25% of total system memory
             #    1024 MB
             [(memory_total_in_mb / 4), 1024].min
           end
new_size =  if node['private_chef']['opscode-solr']['new_size']
              node['private_chef']['opscode-solr']['new_size']
            else
              [(solr_mem / 16), 32].max
            end

java_opts = node['private_chef']['opscode-solr']['java_opts']
java_opts << " -XX:NewSize=#{new_size}M" unless java_opts =~ /NewSize/
java_opts << " -XX:+UseConcMarkSweepGC" unless java_opts =~ /UseConcMarkSweepGC/
java_opts << " -XX:+UseParNewGC" unless java_opts =~ /UseParNewGC/

# Save the values back onto the node attributes
node.default['private_chef']['opscode-solr']['heap_size'] = solr_mem
node.default['private_chef']['opscode-solr']['new_size'] = new_size

node.default['private_chef']['opscode-solr']['command'] =  "java -Xmx#{solr_mem}M -Xms#{solr_mem}M"
node.default['private_chef']['opscode-solr']['command'] << "#{java_opts}"
# Enable GC Logging (very useful for debugging issues)
node.default['private_chef']['opscode-solr']['command'] << " -Xloggc:#{File.join(solr_log_dir, "gclog.log")} -verbose:gc -XX:+PrintHeapAtGC -XX:+PrintGCTimeStamps -XX:+PrintGCDetails -XX:+PrintGCApplicationStoppedTime -XX:+PrintGCApplicationConcurrentTime -XX:+PrintTenuringDistribution"
node.default['private_chef']['opscode-solr']['command'] << " -Dsolr.data.dir=#{solr_data_dir}"
node.default['private_chef']['opscode-solr']['command'] << " -Dsolr.solr.home=#{solr_home_dir}"
node.default['private_chef']['opscode-solr']['command'] << " -server"
node.default['private_chef']['opscode-solr']['command'] << " -jar '#{solr_jetty_dir}/start.jar'"

component_runit_service "opscode-solr"

# log rotation
template "/etc/opscode/logrotate.d/opscode-solr" do
  source "logrotate.erb"
  owner "root"
  group "root"
  mode "0644"
  variables(node['private_chef']['opscode-solr'].to_hash.merge(
    'copytruncate' => true
  ))
end
