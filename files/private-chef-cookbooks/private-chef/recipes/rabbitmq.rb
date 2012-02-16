#
# Author:: Adam Jacob (<adam@opscode.com>)
# Copyright:: Copyright (c) 2011 Opscode, Inc.
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

rabbitmq_dir = node['private_chef']['rabbitmq']['dir']
rabbitmq_etc_dir = File.join(rabbitmq_dir, "etc")
rabbitmq_data_dir = node['private_chef']['rabbitmq']['data_dir']
rabbitmq_data_dir_symlink = File.join(rabbitmq_dir, "db")
rabbitmq_log_dir = node['private_chef']['rabbitmq']['log_directory']

[ rabbitmq_dir, rabbitmq_etc_dir, rabbitmq_data_dir, rabbitmq_log_dir ].each do |dir_name|
  directory dir_name do
    owner node['private_chef']['user']['username']
    mode '0700'
    recursive true
  end
end

link rabbitmq_data_dir_symlink do
  to rabbitmq_data_dir
  not_if { rabbitmq_data_dir_symlink == rabbitmq_data_dir }
end

rabbitmq_service_dir = "/opt/opscode/embedded/service/rabbitmq"

%w[rabbitmqctl rabbitmq-env rabbitmq-multi rabbitmq-server].each do |cmd|
  link "/opt/opscode/embedded/bin/#{cmd}" do
    to File.join(rabbitmq_service_dir, "sbin", cmd)
  end
end

config_file = File.join(node['private_chef']['rabbitmq']['dir'], "etc", "rabbitmq.conf") 

template "#{rabbitmq_service_dir}/sbin/rabbitmq-env" do
  owner "root"
  group "root"
  mode "0755"
  variables( :config_file => config_file )
  source "rabbitmq-env.erb"
end

template config_file do
  source "rabbitmq.conf.erb"
  owner "root"
  group "root"
  mode "0644"
  variables(node['private_chef']['rabbitmq'].to_hash)
end

runit_service "rabbitmq" do
  down node['private_chef']['rabbitmq']['ha']
  options({
    :log_directory => rabbitmq_log_dir
  }.merge(params))
end

add_nagios_hostgroup("rabbitmq")

if node['private_chef']['bootstrap']['enable']
	execute "/opt/opscode/bin/private-chef-ctl rabbitmq start" do
		retries 20 
	end
  
  execute "/opt/opscode/embedded/bin/chpst -u #{node["private_chef"]["user"]["username"]} -U #{node["private_chef"]["user"]["username"]} /opt/opscode/embedded/bin/rabbitmqctl wait /var/opt/opscode/rabbitmq/db/rabbit@localhost.pid" do
    retries 10 
  end

  [ node['private_chef']['rabbitmq']['vhost'], node['private_chef']['rabbitmq']['reindexer_vhost'], node['private_chef']['rabbitmq']['jobs_vhost'] ].each do |vhost|
    execute "/opt/opscode/embedded/bin/rabbitmqctl add_vhost #{vhost}" do
      user node['private_chef']['user']['username']
      not_if "/opt/opscode/embedded/bin/chpst -u #{node["private_chef"]["user"]["username"]} -U #{node["private_chef"]["user"]["username"]} /opt/opscode/embedded/bin/rabbitmqctl list_vhosts| grep #{vhost}"
      retries 20
    end
  end

  # create chef user for the queue
  execute "/opt/opscode/embedded/bin/rabbitmqctl add_user #{node['private_chef']['rabbitmq']['user']} #{node['private_chef']['rabbitmq']['password']}" do
    not_if "/opt/opscode/embedded/bin/chpst -u #{node["private_chef"]["user"]["username"]} -U #{node["private_chef"]["user"]["username"]} /opt/opscode/embedded/bin/rabbitmqctl list_users |grep #{node['private_chef']['rabbitmq']['user']}"
    user node['private_chef']['user']['username']
    retries 10
  end

  execute "/opt/opscode/embedded/bin/rabbitmqctl add_user #{node['private_chef']['rabbitmq']['jobs_user']} #{node['private_chef']['rabbitmq']['jobs_password']}" do
    user node['private_chef']['user']['username']
    not_if "/opt/opscode/embedded/bin/chpst -u #{node["private_chef"]["user"]["username"]} -U #{node["private_chef"]["user"]["username"]} /opt/opscode/embedded/bin/rabbitmqctl list_users |grep #{node['private_chef']['rabbitmq']['jobs_user']}"
    retries 10
  end

  # grant the mapper user the ability to do anything with the /chef vhost
  # the three regex's map to config, write, read permissions respectively
  execute "/opt/opscode/embedded/bin/rabbitmqctl set_permissions -p #{node['private_chef']['rabbitmq']['vhost']} #{node['private_chef']['rabbitmq']['user']} \".*\" \".*\" \".*\"" do
    user node['private_chef']['user']['username']
    not_if "/opt/opscode/embedded/bin/chpst -u #{node["private_chef"]["user"]["username"]} -U #{node["private_chef"]["user"]["username"]} /opt/opscode/embedded/bin/rabbitmqctl list_user_permissions #{node['private_chef']['rabbitmq']['user']}|grep #{node['private_chef']['rabbitmq']['vhost']}"
    retries 10
  end

  execute "/opt/opscode/embedded/bin/rabbitmqctl set_permissions -p #{node['private_chef']['rabbitmq']['reindexer_vhost']} #{node['private_chef']['rabbitmq']['user']} \".*\" \".*\" \".*\"" do
    user node['private_chef']['user']['username']
    not_if "/opt/opscode/embedded/bin/chpst -u #{node["private_chef"]["user"]["username"]} -U #{node["private_chef"]["user"]["username"]} /opt/opscode/embedded/bin/rabbitmqctl list_user_permissions #{node['private_chef']['rabbitmq']['user']}|grep #{node['private_chef']['rabbitmq']['reindexer_vhost']}"
    retries 10
  end

  execute "/opt/opscode/embedded/bin/rabbitmqctl set_permissions -p #{node['private_chef']['rabbitmq']['jobs_vhost']} #{node['private_chef']['rabbitmq']['jobs_user']} \".*\" \".*\" \".*\"" do
    user node['private_chef']['user']['username']
    not_if "/opt/opscode/embedded/bin/chpst -u #{node["private_chef"]["user"]["username"]} -U #{node["private_chef"]["user"]["username"]} /opt/opscode/embedded/bin/rabbitmqctl list_user_permissions #{node['private_chef']['rabbitmq']['jobs_user']}|grep #{node['private_chef']['rabbitmq']['jobs_vhost']}"
    retries 10
  end
end


