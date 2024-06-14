# Copyright:: Chef Software, Inc.
# All Rights Reserved

MAX_MAP_COUNT = 262_144
cluster_name = if node['previous_run'] && node['previous_run']['opensearch'] && node['previous_run']['opensearch']['cluster_name']
                 node['previous_run']['opensearch']['cluster_name']
               else
                 "ChefInfraServer-#{SecureRandom.hex(4)}"
               end

opensearch_password = node['private_chef']['opscode-erchef']['search_auth_password'] || PrivateChef.credentials.get('opscode_erchef', 'search_auth_password')

node.override['private_chef']['opensearch']['cluster_name'] = cluster_name
opensearch = node['private_chef']['opensearch']

opensearch_dir              = opensearch['dir']                   # /var/opt/opscode/opensearch
opensearch_conf_dir         = File.join(opensearch_dir, 'config') # /var/opt/opscode/opensearch/config
opensearch_data_dir         = opensearch['data_dir']              # /var/opt/opscode/opensearch/data
opensearch_plugins_dir      = opensearch['plugins_directory']     # /var/opt/opscode/opensearch/plugins
opensearch_scripts_dir      = opensearch['scripts_directory']     # /var/opt/opscode/opensearch/scripts
opensearch_temp_dir         = opensearch['temp_directory']        # /var/log/opscode/opensearch/tmp
opensearch_log_dir          = opensearch['log_directory']         # /var/log/opscode/opensearch
opensearch_env_dir          = opensearch['env_directory']         # /opt/opscode/service/opensearch/env
opensearch_opt_conf_dir     = opensearch['opt_conf_directory']    # /opt/opscode/embedded/opensearch/config
opensearch_opt_plugins_dir  = opensearch['opt_plugins_directory'] # /opt/opscode/embedded/opensearch/plugins

config_file = File.join(opensearch_conf_dir, 'opensearch.yml')
logging_config_file = File.join(opensearch_conf_dir, 'logging.yml')

# set up the basic es directory structure
[ opensearch_dir,
  opensearch_data_dir,
  opensearch_temp_dir,
  opensearch_log_dir,
  opensearch_conf_dir,
  opensearch_plugins_dir,
  opensearch_scripts_dir,
].each do |dir_name|
  directory dir_name do
    owner OmnibusHelper.new(node).ownership['owner']
    group OmnibusHelper.new(node).ownership['group']
    mode node['private_chef']['service_dir_perms']
    recursive true
  end
end

execute 'sysctl-reload' do
  command '/sbin/sysctl -p /etc/sysctl.conf || true'
  action :nothing
end

# Just make sure the file is there, saves a round of error handling
# when we open it up.
file '/etc/sysctl.conf' do
  user 'root'
  action :create
end

sysctl 'vm.max_map_count' do
  value MAX_MAP_COUNT.to_s
  only_if do
    # This is fairly cautious. Because we have no guarantees around what
    # might already be in sysctl.conf, we'll assume multiple matches are
    # possible for the value we want.  We'll find them all and see if the highest
    # is high enough.
    # We will also look for the value in sysctl.conf instead of
    # querying sysctl - a different value that won't persist could have been
    # set by the operator, causing problems for ES on next reboot.
    # TODO: foundation for a shared sysctl resource?
    highest_val = 0
    File.read('/etc/sysctl.conf').split("\n").each do |line|
      line.chomp!
      match = /^[[:space:]]*vm\.max_map_count[[:space:]]*=[[:space:]]*([[:digit:]]+)/.match(line)
      if match
        val = match[1].to_i
        highest_val = val if val > highest_val
      end
    end
    highest_val < MAX_MAP_COUNT
  end
  notifies :run, 'execute[sysctl-reload]', :immediately
end

# Remove the old env config to ensure it's not left over after an upgrade.
directory opensearch_env_dir do
  action :delete
  recursive true
end

template config_file do
  source 'opensearch.yml.erb'
  owner OmnibusHelper.new(node).ownership['owner']
  group OmnibusHelper.new(node).ownership['group']
  mode '0644'
  variables(lazy { opensearch.to_hash })
  force_unlink true
  notifies :restart, 'component_runit_service[opensearch]', :delayed
end

template logging_config_file do
  source 'opensearch_logging.yml.erb'
  owner OmnibusHelper.new(node).ownership['owner']
  group OmnibusHelper.new(node).ownership['group']
  mode '0644'
  variables(opensearch.to_hash)
  force_unlink true
  notifies :restart, 'component_runit_service[opensearch]', :delayed
end

heap_size = opensearch['heap_size']

jvm_config_file = File.join(opensearch_conf_dir, 'jvm.options')
template jvm_config_file do
  source 'opensearch_jvm.opts.erb'
  owner OmnibusHelper.new(node).ownership['owner']
  group OmnibusHelper.new(node).ownership['group']
  mode '0644'
  variables(jvm_opts: opensearch['jvm_opts'],
            log_dir: opensearch['log_directory'],
            heap_size: heap_size,
            new_size: opensearch['new_size'],
            enable_gc_log: opensearch['enable_gc_log'],
            tmp_dir: opensearch['temp_directory'])
  notifies :restart, 'component_runit_service[opensearch]'
end

log4j_file = File.join(opensearch_conf_dir, 'log4j2.properties')
cookbook_file log4j_file do
  source 'opensearch/es_log4j2.properties'
  owner OmnibusHelper.new(node).ownership['owner']
  group OmnibusHelper.new(node).ownership['group']
  mode '0744'
end

# We are not enabling SSL for http but it is mandatory for transport layer.
# So we are generating them as opendistro security plugin.
# In future we can enable them If required.
ssl_script = File.join(opensearch_conf_dir, 'ssl_script.sh')
cookbook_file ssl_script do
  source 'opensearch/ssl_script.sh'
  owner OmnibusHelper.new(node).ownership['owner']
  group OmnibusHelper.new(node).ownership['group']
  mode '0744'
end

execute 'generate ssl certificates' do
  command "sh #{ssl_script}"
  cwd opensearch_conf_dir
  user OmnibusHelper.new(node).ownership['owner']
  not_if { ::File.exist?("#{opensearch_conf_dir}/admin.pem") }
end

template "#{opensearch_opt_plugins_dir}/opensearch-security/securityconfig/internal_users.yml" do
  source 'opensearch_internal_users.yml.erb'
  owner OmnibusHelper.new(node).ownership['owner']
  group OmnibusHelper.new(node).ownership['group']
  mode '0644'
  variables(username: node['private_chef']['opscode-erchef']['search_auth_username'],
            password: OmnibusHelper.new(node).hash_password(opensearch_password))
  notifies :restart, 'component_runit_service[opensearch]'
end

# It appears that the jvm config file must be in the
# install directory.  We'll keep it in the config location
# for consistency, but symlink the config dir over to the
# install path. It looks in the configured config path
# (in /var/opt) after it starts up - but so far I have not
# found a way to change where it looks for jvm.options
# It appears to be hard-coded to look in the install path/config,
# then in /etc/opensearch.
#
# TODO - what happens on upgrade? Will the config disappear until reconfigure?
#
link opensearch_opt_conf_dir do
  to opensearch_conf_dir
end

# While Restoring the chef server backup, enable is always successful
# but start is not which is the root cause of restore issue.
# So added retries for making sure the opensearch runit service is running
component_runit_service 'opensearch' do
  action [:enable, :start]
  retries 10
  retry_delay 1
end

chef_sleep 10

execute 'add internal user to opensearch security plugin' do
  command <<~EOF
    export JAVA_HOME="/opt/#{ChefUtils::Dist::Org::LEGACY_CONF_DIR}/embedded/open-jre/";
    ./securityadmin.sh -f ../securityconfig/internal_users.yml -icl -nhnv \
    -cert #{opensearch_opt_conf_dir}/admin.pem \
    -cacert #{opensearch_opt_conf_dir}/root-ca.pem \
    -key #{opensearch_opt_conf_dir}/admin-key.pem
  EOF
  cwd "#{opensearch_opt_plugins_dir}/opensearch-security/tools/"
  user OmnibusHelper.new(node).ownership['owner']
  retries 10
  retry_delay 1
end

include_recipe 'infra-server::opensearch_index'
