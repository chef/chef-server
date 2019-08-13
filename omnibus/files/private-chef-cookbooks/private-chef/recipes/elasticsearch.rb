# Copyright 2019 Chef Software, Inc.
# All Rights Reserved

MAX_MAP_COUNT = 262_144

elasticsearch = node['private_chef']['elasticsearch']

elasticsearch_dir              = elasticsearch['dir']                   # /var/opt/opscode/elasticsearch
elasticsearch_conf_dir         = File.join(elasticsearch_dir, "config") # /var/opt/opscode/elasticsearch/config
elasticsearch_data_dir         = elasticsearch['data_dir']              # /var/opt/opscode/elasticsearch/data
elasticsearch_plugins_dir      = elasticsearch['plugins_directory']     # /var/opt/opscode/elasticsearch/plugins
elasticsearch_scripts_dir      = elasticsearch['scripts_directory']     # /var/opt/opscode/elasticsearch/scripts
elasticsearch_temp_dir         = elasticsearch['temp_directory']        # /var/log/opscode/elasticsearch/tmp
elasticsearch_log_dir          = elasticsearch['log_directory']         # /var/log/opscode/elasticsearch

config_file = File.join(elasticsearch_conf_dir, "elasticsearch.yml")
logging_config_file = File.join(elasticsearch_conf_dir, "logging.yml")

# set up the basic es directory structure
[ elasticsearch_dir,
  elasticsearch_data_dir,
  elasticsearch_temp_dir,
  elasticsearch_log_dir,
  elasticsearch_conf_dir,
  elasticsearch_plugins_dir,
  elasticsearch_scripts_dir,
].each do |dir_name|
  directory dir_name do
    owner OmnibusHelper.new(node).ownership['owner']
    group OmnibusHelper.new(node).ownership['group']
    mode node['private_chef']['service_dir_perms']
    recursive true
  end
end

execute "sysctl-reload" do
  command "/sbin/sysctl -p /etc/sysctl.conf || true"
  action :nothing
end

# Just make sure the file is there, saves a round of error handling
# when we open it up.
file "/etc/sysctl.conf" do
  user "root"
  action :touch
  not_if { File.exists?("/etc/sysctl.conf") }
end

# Ideally, we'd just render a template into sysctl.d - but that's
# 6.2 and later under centos. This method will work for all supported
# platforms.
bash "increase vm.max_map_count" do
  user "root"
  code <<-EOF
     # Now that we've made the determination, remove any other max_map_count lines
     # from the file, since we're about to put a new one in
    egrep -v "^[[:space:]]*vm\.max_map_count[[:space:]]*=" /etc/sysctl.conf > /tmp/chef-new-sysctl.conf || exit 1
    cp /tmp/chef-new-sysctl.conf /etc/sysctl.conf
    echo 'vm.max_map_count = #{MAX_MAP_COUNT}' >> /etc/sysctl.conf
  EOF
  notifies :run, 'execute[sysctl-reload]', :immediately

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
    File.read("/etc/sysctl.conf").split("\n").each do |line|
      line.chomp!
      match = /^[[:space:]]*vm\.max_map_count[[:space:]]*=[[:space:]]*([[:digit:]]+)/.match(line)
      if match
        val = match[1].to_i
        highest_val = val if val > highest_val
      end
    end
    highest_val < MAX_MAP_COUNT
  end
end

cluster_name = "ChefInfraServer-#{SecureRandom.hex(4)}"

# No discovery settings since we will have only one chef-server backend node.

# Remove the old env config to ensre it's not left over after an upgrade.
directory "/opt/opscode/service/elasticsearch/env" do
  action :delete
  recursive true
end
template config_file do
  source "elasticsearch.yml.erb"
  owner OmnibusHelper.new(node).ownership['owner']
  group OmnibusHelper.new(node).ownership['group']
  mode "0644"
  variables (lazy { elasticsearch.to_hash.merge({ cluster_name: cluster_name }) })
  force_unlink true
  notifies :restart, 'component_runit_service[elasticsearch]', :delayed
end

template logging_config_file do
  source "elasticsearch_logging.yml.erb"
  owner OmnibusHelper.new(node).ownership['owner']
  group OmnibusHelper.new(node).ownership['group']
  mode "0644"
  variables(elasticsearch.to_hash)
  force_unlink true
  notifies :restart, 'component_runit_service[elasticsearch]', :delayed
end
jvm_config_file = File.join(elasticsearch_conf_dir, "jvm.options")
template jvm_config_file do
  source "elasticsearch_jvm.opts.erb"
  owner OmnibusHelper.new(node).ownership['owner']
  group OmnibusHelper.new(node).ownership['group']
  mode "0644"
  variables(jvm_opts: elasticsearch['jvm_opts'],
            log_dir: elasticsearch['log_diriectory'],
            heap_size: elasticsearch['heap_size'],
            new_size: elasticsearch['new_size'],
            enable_gc_log: elasticsearch['enable_gc_log'],
            tmp_dir: elasticsearch['temp_directory']
            )
  notifies :restart, 'component_runit_service[elasticsearch]'
end

log4j_file = File.join(elasticsearch_conf_dir, "log4j2.properties")
cookbook_file log4j_file do
  source "elasticsearch/es_log4j2.properties"
  owner OmnibusHelper.new(node).ownership['owner']
  group OmnibusHelper.new(node).ownership['group']
  mode "0744"
end


# It appears that the jvm config file must be in the
# install directory.  We'll keep it in the config location
# for consistency, but symlink the config dir over to the
# install path. It looks in the configured config path
# (in /var/opt) after it starts up - but so far I have not
# found a way to change where it looks for jvm.options
# It appears to be hard-coded to look in the install path/config,
# then in /etc/elasticsearch.
#
# TODO - what happens on upgrade? Will the config disappear until reconfigure?
#
link "/opt/opscode/embedded/elasticsearch/config" do
  to elasticsearch_conf_dir
end

#Define resource for elasticsearch component_runit_service
component_runit_service "elasticsearch"
