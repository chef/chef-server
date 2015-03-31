#
# Author:: Adam Jacob (<adam@chef.io>)
# Author:: Tyler Cloke (<tyler@chef.io>)
# Copyright:: Copyright (c) 2011-2015 Opscode, Inc.
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

# path needed for rabbitmqctl to run
rabbitmq_path = rabbitmq['env_path']
rabbitmq_env = ENV.to_hash.merge("PATH" => rabbitmq_path)

[ rabbitmq_dir, rabbitmq_etc_dir, rabbitmq_data_dir, rabbitmq_log_dir ].each do |dir_name|
  directory dir_name do
    owner OmnibusHelper.new(node).ownership['owner']
    group OmnibusHelper.new(node).ownership['group']
    mode node['private_chef']['service_dir_perms']
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
%w[rabbitmqctl rabbitmq-defaults rabbitmq-env rabbitmq-plugins rabbitmq-server].each do |cmd|
  link "/opt/opscode/embedded/bin/#{cmd}" do
    to File.join(rabbitmq_service_dir, "sbin", cmd)
  end
end

config_file = File.join(rabbitmq_etc_dir, "rabbitmq.conf")

template "#{rabbitmq_service_dir}/sbin/rabbitmq-defaults" do
  owner "root"
  group "root"
  mode "0755"
  variables( :rabbitmq_dir => rabbitmq_dir,
             :config_file => config_file )
end

template config_file do
  source "rabbitmq.conf.erb"
  owner "root"
  group "root"
  mode "0644"
  variables(rabbitmq.to_hash)
end

component_runit_service "rabbitmq"

if is_data_master?
  rmq_ctl = "/opt/opscode/embedded/bin/rabbitmqctl"
  opc_ctl = "/opt/opscode/bin/private-chef-ctl"
  opc_username = OmnibusHelper.new(node).ownership['owner']
  rmq_ctl_chpost = "/opt/opscode/embedded/bin/chpst -u #{opc_username} -U #{opc_username} #{rmq_ctl}"

  execute "#{opc_ctl} start rabbitmq" do
    environment rabbitmq_env
    retries 20
  end

  execute "#{rmq_ctl_chpost} wait #{rabbitmq_data_dir}/#{rabbitmq['nodename']}.pid" do
    environment rabbitmq_env
    retries 10
  end

  [ rabbitmq['vhost'], rabbitmq['reindexer_vhost'], rabbitmq['jobs_vhost'], rabbitmq['actions_vhost'] ].each do |vhost|
    execute "#{rmq_ctl} add_vhost #{vhost}" do
      environment (rabbitmq_env)
      user opc_username
      not_if "#{rmq_ctl_chpost} list_vhosts| grep #{vhost}", :environment => rabbitmq_env, :user => "root"
      retries 20
    end
  end
  # create chef user for the queue
  execute "#{rmq_ctl} add_user #{rabbitmq['user']} #{rabbitmq['password']}" do
    environment (rabbitmq_env)
    not_if "#{rmq_ctl_chpost} list_users |grep #{rabbitmq['user']}", :environment => rabbitmq_env, :user => "root"
    user opc_username
    retries 10
  end

  execute "#{rmq_ctl} add_user #{rabbitmq['jobs_user']} #{rabbitmq['jobs_password']}" do
    environment (rabbitmq_env)
    user opc_username
    not_if "#{rmq_ctl_chpost} list_users |grep #{rabbitmq['jobs_user']}", :environment => rabbitmq_env, :user => "root"
    retries 10
  end

  execute "#{rmq_ctl} add_user #{rabbitmq['actions_user']} #{rabbitmq['actions_password']}" do
    environment (rabbitmq_env)
    user opc_username
    not_if "#{rmq_ctl_chpost} list_users |grep #{rabbitmq['actions_user']}", :environment => rabbitmq_env, :user => "root"
    retries 10
  end

  #

  # grant the mapper user the ability to do anything with the /chef vhost
  # the three regex's map to config, write, read permissions respectively
  #
  execute "#{rmq_ctl} set_permissions -p #{rabbitmq['vhost']} #{rabbitmq['user']} \".*\" \".*\" \".*\"" do
    environment (rabbitmq_env)
    user opc_username
    not_if "#{rmq_ctl_chpost} list_user_permissions #{rabbitmq['user']}|grep #{rabbitmq['vhost']}", :environment => rabbitmq_env, :user => "root"
    retries 10
  end

  execute "#{rmq_ctl} set_permissions -p #{rabbitmq['reindexer_vhost']} #{rabbitmq['user']} \".*\" \".*\" \".*\"" do
    environment (rabbitmq_env)
    user opc_username
    not_if "#{rmq_ctl_chpost} list_user_permissions #{rabbitmq['user']}|grep #{rabbitmq['reindexer_vhost']}", :environment => rabbitmq_env, :user => "root"
    retries 10
  end

  execute "#{rmq_ctl} set_permissions -p #{rabbitmq['jobs_vhost']} #{rabbitmq['jobs_user']} \".*\" \".*\" \".*\"" do
    environment (rabbitmq_env)
    user opc_username
    not_if "#{rmq_ctl_chpost} list_user_permissions #{rabbitmq['jobs_user']}|grep #{rabbitmq['jobs_vhost']}", :environment => rabbitmq_env, :user => "root"
    retries 10
  end

  execute "#{rmq_ctl} set_permissions -p #{rabbitmq['actions_vhost']} #{rabbitmq['user']} \".*\" \".*\" \".*\"" do
    environment (rabbitmq_env)
    user opc_username
    not_if "#{rmq_ctl_chpost} list_user_permissions #{rabbitmq['user']}|grep #{rabbitmq['actions_vhost']}", :environment => rabbitmq_env, :user => "root"
    retries 10
  end

  execute "#{rmq_ctl} set_permissions -p #{rabbitmq['actions_vhost']} #{rabbitmq['actions_user']} \".*\" \".*\" \".*\"" do
    environment (rabbitmq_env)
    user opc_username
    not_if "#{rmq_ctl_chpost} list_user_permissions #{rabbitmq['actions_user']}|grep #{rabbitmq['actions_vhost']}", :environment => rabbitmq_env, :user => "root"
    retries 10
  end
end
