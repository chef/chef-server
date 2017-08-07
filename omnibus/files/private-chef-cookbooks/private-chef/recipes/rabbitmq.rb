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

password = PrivateChef.credentials.get("rabbitmq", "password")
actions_password = PrivateChef.credentials.get("rabbitmq", "actions_password")
management_password = PrivateChef.credentials.get("rabbitmq", "management_password")

rabbitmq_dir = rabbitmq['dir']
rabbitmq_etc_dir = File.join(rabbitmq_dir, "etc")
rabbitmq_ca_dir = rabbitmq_etc_dir
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

# reuse nginx certs for RabbitMQ Management Plugin
nginx_dir = node['private_chef']['nginx']['dir']
nginx_ca_dir = File.join(nginx_dir, 'ca')
ssl_keyfile = File.join(nginx_ca_dir, "#{node['private_chef']['nginx']['server_name']}.key")
ssl_crtfile = File.join(nginx_ca_dir, "#{node['private_chef']['nginx']['server_name']}.crt")

ssl_versions = node['private_chef']['rabbitmq']['ssl_versions'].map{ |v| "'#{v}'"}.join(",")

template "#{rabbitmq_etc_dir}/rabbitmq.config" do
  owner "root"
  group "root"
  mode "0755"
  variables( :ssl_keyfile => ssl_keyfile,
             :ssl_crtfile => ssl_crtfile,
             :ssl_versions => ssl_versions)
end

component_runit_service "rabbitmq" do
  runit_attributes(check: true)
  control ['t']
end

if is_data_master?
  rmq_ctl = "/opt/opscode/embedded/bin/rabbitmqctl"
  rmq_plugins = "/opt/opscode/embedded/bin/rabbitmq-plugins"
  opc_ctl = "/opt/opscode/bin/private-chef-ctl"
  opc_username = OmnibusHelper.new(node).ownership['owner']
  rmq_ctl_chpst = "/opt/opscode/embedded/bin/chpst -u #{opc_username} -U #{opc_username} #{rmq_ctl}"
  rmq_plugins_chpst = "/opt/opscode/embedded/bin/chpst -u #{opc_username} -U #{opc_username} #{rmq_plugins}"

  execute "#{opc_ctl} start rabbitmq" do
    environment rabbitmq_env
    retries 20
  end

  execute "#{rmq_ctl_chpst} wait #{rabbitmq_data_dir}/#{rabbitmq['nodename']}.pid" do
    environment rabbitmq_env
    retries 10
  end

  [ rabbitmq['vhost'], rabbitmq['actions_vhost'] ].each do |vhost|
    execute "#{rmq_ctl} add_vhost #{vhost}" do
      environment (rabbitmq_env)
      user opc_username
      not_if "#{rmq_ctl_chpst} list_vhosts| grep #{vhost}", :environment => rabbitmq_env, :user => "root"
      retries 20
    end
  end

  # create chef user for the queue
  execute "#{rmq_ctl} add_user #{rabbitmq['user']} [PASSWORD]" do
    command "#{rmq_ctl} add_user #{rabbitmq['user']} #{password}"
    environment (rabbitmq_env)
    not_if "#{rmq_ctl_chpst} list_users |grep #{rabbitmq['user']}", :environment => rabbitmq_env, :user => "root"
    user opc_username
    retries 10
    sensitive true
  end

  execute "#{rmq_ctl} add_user #{rabbitmq['actions_user']} [PASSWORD]" do
    command "#{rmq_ctl} add_user #{rabbitmq['actions_user']} #{actions_password}"
    environment (rabbitmq_env)
    user opc_username
    not_if "#{rmq_ctl_chpst} list_users |grep #{rabbitmq['actions_user']}", :environment => rabbitmq_env, :user => "root"
    retries 10
    sensitive true
  end

  execute "#{rmq_ctl} add_user #{rabbitmq['management_user']} [PASSWORD]" do
    command "#{rmq_ctl} add_user #{rabbitmq['management_user']} #{management_password}"
    environment (rabbitmq_env)
    user opc_username
    not_if "#{rmq_ctl_chpst} list_users |grep #{rabbitmq['management_user']}", :environment => rabbitmq_env, :user => "root"
    retries 10
    sensitive true
  end

  # Update the passwords if they've changed (we'll notice by trying to
  # authenticate the user with the (possibly) new password)
  execute "#{rmq_ctl} change_password #{rabbitmq['user']} [PASSWORD]" do
    command "#{rmq_ctl} change_password #{rabbitmq['user']} #{password}"
    environment (rabbitmq_env)
    user opc_username
    not_if "#{rmq_ctl_chpst} authenticate_user #{rabbitmq['user']} #{password}", :environment => rabbitmq_env, :user => "root"
    retries 10
    sensitive true
  end

  execute "#{rmq_ctl} change_password #{rabbitmq['actions_user']} [PASSWORD]" do
    command "#{rmq_ctl} change_password #{rabbitmq['actions_user']} #{actions_password}"
    environment (rabbitmq_env)
    user opc_username
    not_if "#{rmq_ctl_chpst} authenticate_user #{rabbitmq['actions_user']} #{actions_password}", :environment => rabbitmq_env, :user => "root"
    retries 10
    sensitive true
  end

  execute "#{rmq_ctl} change_password #{rabbitmq['management_user']} [PASSWORD]" do
    command "#{rmq_ctl} change_password #{rabbitmq['management_user']} #{management_password}"
    environment (rabbitmq_env)
    user opc_username
    not_if "#{rmq_ctl_chpst} authenticate_user #{rabbitmq['management_user']} #{management_password}", :environment => rabbitmq_env, :user => "root"
    retries 10
    sensitive true
  end

  #
  # grant the mapper user the ability to do anything with the /chef vhost
  # the three regex's map to config, write, read permissions respectively
  #
  execute "#{rmq_ctl} set_permissions -p #{rabbitmq['vhost']} #{rabbitmq['user']} \".*\" \".*\" \".*\"" do
    environment (rabbitmq_env)
    user opc_username
    not_if "#{rmq_ctl_chpst} list_user_permissions #{rabbitmq['user']}|grep #{rabbitmq['vhost']}", :environment => rabbitmq_env, :user => "root"
    retries 10
  end

  execute "#{rmq_ctl} set_permissions -p #{rabbitmq['actions_vhost']} #{rabbitmq['user']} \".*\" \".*\" \".*\"" do
    environment (rabbitmq_env)
    user opc_username
    not_if "#{rmq_ctl_chpst} list_user_permissions #{rabbitmq['user']}|grep #{rabbitmq['actions_vhost']}", :environment => rabbitmq_env, :user => "root"
    retries 10
  end

  execute "#{rmq_ctl} set_permissions -p #{rabbitmq['actions_vhost']} #{rabbitmq['actions_user']} \".*\" \".*\" \".*\"" do
    environment (rabbitmq_env)
    user opc_username
    not_if "#{rmq_ctl_chpst} list_user_permissions #{rabbitmq['actions_user']}|grep #{rabbitmq['actions_vhost']}", :environment => rabbitmq_env, :user => "root"
    retries 10
  end


  execute "#{rmq_ctl} set_permissions -p #{rabbitmq['actions_vhost']} #{rabbitmq['management_user']} \".*\" \".*\" \".*\"" do
    environment (rabbitmq_env)
    user opc_username
    not_if "#{rmq_ctl_chpst} list_user_permissions #{rabbitmq['management_user']}|grep #{rabbitmq['actions_vhost']}", :environment => rabbitmq_env, :user => "root"
    retries 10
  end

  execute "#{rmq_ctl} set_permissions -p #{rabbitmq['vhost']} #{rabbitmq['management_user']} \".*\" \".*\" \".*\"" do
    environment (rabbitmq_env)
    user opc_username
    not_if "#{rmq_ctl_chpst} list_user_permissions #{rabbitmq['management_user']}|grep #{rabbitmq['vhost']}", :environment => rabbitmq_env, :user => "root"
    retries 10
  end

  execute "#{rmq_ctl} set_permissions -p / #{rabbitmq['management_user']} \".*\" \".*\" \".*\"" do
    environment (rabbitmq_env)
    user opc_username
    not_if "#{rmq_ctl_chpst} list_user_permissions #{rabbitmq['management_user']}|grep \"/\\s\"", :environment => rabbitmq_env, :user => "root"
    retries 10
  end

  rabbitmq_management_is_up = "#{rmq_plugins} list | grep rabbitmq_management  | grep -v rabbitmq_management_agent | grep -v rabbitmq_management_visualiser | grep E"
  if rabbitmq['management_enabled']
    execute "#{rmq_plugins} enable rabbitmq_management" do
      environment (rabbitmq_env)
      user opc_username
      not_if rabbitmq_management_is_up
      # management plugin needs a rabbit restart
      notifies :restart, 'runit_service[rabbitmq]', :delayed
      retries 10
    end
  else
    execute "#{rmq_plugins} disable rabbitmq_management" do
      environment (rabbitmq_env)
      user opc_username
      notifies :restart, 'runit_service[rabbitmq]', :delayed
      only_if rabbitmq_management_is_up
      retries 10
    end
  end

  execute "#{rmq_ctl} set_user_tags #{rabbitmq['management_user']} administrator" do
    environment (rabbitmq_env)
    user opc_username
    not_if "#{rmq_ctl_chpst} list_users | grep #{rabbitmq['management_user']} | grep administrator", :environment => rabbitmq_env, :user => "root"
    retries 10
  end

  execute "#{rmq_ctl} set_policy -p /analytics max_length '(erchef|alaska|notifier.notifications|notifier_config)' '{\"max-length\":#{rabbitmq['analytics_max_length']}}' --apply-to queues" do
    environment (rabbitmq_env)
    user opc_username
    only_if do rabbitmq['analytics_max_length'] > 0 end
    retries 10
  end

  execute "#{rmq_ctl} clear_policy -p /analytics max_length" do
    environment (rabbitmq_env)
    user opc_username
    not_if do rabbitmq['analytics_max_length'] > 0 end
    retries 10
  end

end
