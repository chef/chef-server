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

rabbitmq = node["private_chef"]["rabbitmq"]

rabbitmq_dir = rabbitmq['dir']
rabbitmq_etc_dir = File.join(rabbitmq_dir, "etc")
rabbitmq_data_dir = rabbitmq['data_dir']
rabbitmq_data_dir_symlink = File.join(rabbitmq_dir, "db")
rabbitmq_log_dir = rabbitmq['log_directory']

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

######################################################################
# NOTE:
# we do the symlinking in the build, but we're just making sure that
# the links are still there in the cookbook
######################################################################
%w[rabbitmqctl rabbitmq-env rabbitmq-multi rabbitmq-server].each do |cmd|
  link "/opt/opscode/embedded/bin/#{cmd}" do
    to File.join(rabbitmq_service_dir, "sbin", cmd)
  end
end

config_file = File.join(rabbitmq['dir'], "etc", "rabbitmq.conf") 

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
  variables(rabbitmq.to_hash)
end

runit_service "rabbitmq" do
  down rabbitmq['ha']
  options({
    :log_directory => rabbitmq_log_dir,
    :svlogd_size => rabbitmq['svlogd_size'],
    :svlogd_num  => rabbitmq['svlogd_num']
  }.merge(params))
end

add_nagios_hostgroup("rabbitmq")

if node['private_chef']['bootstrap']['enable']
  rmq_ctl = "/opt/opscode/embedded/bin/rabbitmqctl"
  opc_ctl = "/opt/opscode/bin/private-chef-ctl"
  opc_username = node["private_chef"]["user"]["username"]
  rmq_ctl_chpost = "/opt/opscode/embedded/bin/chpst -u #{opc_username} -U #{opc_username} #{rmq_ctl}"

  execute "/opt/opscode/bin/private-chef-ctl rabbitmq start" do
    retries 20 
  end
  
  execute "#{rmq_ctl_chpost} wait #{rabbitmq_data_dir}/rabbit@localhost.pid" do
    retries 10 
  end

  [ rabbitmq['vhost'], rabbitmq['reindexer_vhost'], rabbitmq['jobs_vhost'], rabbitmq['reports_vhost'] ].each do |vhost|
    execute "#{rmq_ctl} add_vhost #{vhost}" do
      user node['private_chef']['user']['username']
      not_if "#{rmq_ctl_chpost} list_vhosts| grep #{vhost}"
      retries 20
    end
  end
  # create chef user for the queue
  execute "#{rmq_ctl} add_user #{rabbitmq['user']} #{rabbitmq['password']}" do
    not_if "#{rmq_ctl_chpost} list_users |grep #{rabbitmq['user']}"
    user node['private_chef']['user']['username']
    retries 10
  end

  execute "#{rmq_ctl} add_user #{rabbitmq['jobs_user']} #{rabbitmq['jobs_password']}" do
    user node['private_chef']['user']['username']
    not_if "#{rmq_ctl_chpost} list_users |grep #{rabbitmq['jobs_user']}"
    retries 10
  end
  
  execute "#{rmq_ctl} add_user #{rabbitmq['reports_user']} #{rabbitmq['reports_password']}" do
    user node['private_chef']['user']['username']
    not_if "#{rmq_ctl_chpost} list_users |grep #{rabbitmq['reports_user']}"
    retries 10
  end
  #

  # grant the mapper user the ability to do anything with the /chef vhost
  # the three regex's map to config, write, read permissions respectively
  #
  execute "#{rmq_ctl} set_permissions -p #{rabbitmq['vhost']} #{rabbitmq['user']} \".*\" \".*\" \".*\"" do
    user node['private_chef']['user']['username']
    not_if "#{rmq_ctl_chpost} list_user_permissions #{rabbitmq['user']}|grep #{rabbitmq['vhost']}"
    retries 10
  end

  execute "#{rmq_ctl} set_permissions -p #{rabbitmq['reindexer_vhost']} #{rabbitmq['user']} \".*\" \".*\" \".*\"" do
    user node['private_chef']['user']['username']
    not_if "#{rmq_ctl_chpost} list_user_permissions #{rabbitmq['user']}|grep #{rabbitmq['reindexer_vhost']}"
    retries 10
  end

  execute "#{rmq_ctl} set_permissions -p #{rabbitmq['jobs_vhost']} #{rabbitmq['jobs_user']} \".*\" \".*\" \".*\"" do
    user node['private_chef']['user']['username']
    not_if "#{rmq_ctl_chpost} list_user_permissions #{rabbitmq['jobs_user']}|grep #{rabbitmq['jobs_vhost']}"
    retries 10
  end

  execute "#{rmq_ctl} set_permissions -p #{rabbitmq['reports_vhost']} #{rabbitmq['reports_user']} \".*\" \".*\" \".*\"" do
    user node['private_chef']['user']['username']
    not_if "#{rmq_ctl_chpost} list_user_permissions #{rabbitmq['reports_user']}|grep #{rabbitmq['reports_vhost']}"
    retries 10
  end
end


