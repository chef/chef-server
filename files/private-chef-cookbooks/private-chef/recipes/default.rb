#
# Copyright:: Copyright (c) 2012 Opscode, Inc.
# Author:: Adam Jacob (<adam@opscode.com>)
#

require 'uuidtools'
require 'openssl'

# Because these symlinks get removed during the postrm
# of the chef-server and private-chef packages, we should
# ensure that they're always here.
%w{private-chef-ctl chef-server-ctl}.each do |bin|
  link "/usr/bin/#{bin}" do
    to "/opt/opscode/bin/#{bin}"
  end
end

# Ensure that all our Omnibus-ed binaries are the ones that get used;
# much better than having to specify this on each resource!
ENV['PATH'] = "/opt/opscode/bin:/opt/opscode/embedded/bin:#{ENV['PATH']}"

# Capture old node attribute values (if there are any, that is) in
# case we need them for comparison purposes for making changes or
# updates
#
# TODO: extract this into something that add-ons can use; no sense
# cargo-culting it around everywhere
if File.exists?("/etc/opscode/chef-server-running.json")
  old_config = JSON.parse(IO.read("/etc/opscode/chef-server-running.json"))

  # We're stashing these outside the "private_chef" attributes tree to
  # prevent us from carrying them along forever when we write out the
  # chef-server-running.json file at the end of the run.
  #
  # For example, to access the old version of the attribute
  # ['private_chef']['foo']['bar'], you'll look at
  # ['previous_run']['foo']['bar'].
  #
  # Take care to check that ['previous_run'] exists, though, otherwise
  # you'll run into trouble doing the initial Chef run.
  #
  # TODO: Provide an API for getting this information
  node.consume_attributes({"previous_run" => old_config['private_chef']})
end

directory "/etc/opscode" do
  owner "root"
  group "root"
  mode "0755"
  action :nothing
end.run_action(:create)

directory "/etc/opscode/logrotate.d" do
  owner "root"
  group "root"
  mode "0755"
  action :nothing
end.run_action(:create)

if File.exists?("/etc/opscode/chef-server.json")
  Chef::Log.warn("Please move to /etc/opscode/chef-server.rb for configuration - /etc/opscode/chef-server.json is deprecated.")
else
  PrivateChef[:node] = node
  private_chef_path = "/etc/opscode/private-chef.rb"
  chef_server_path = "/etc/opscode/chef-server.rb"
  private_chef_rb_exists = File.exists?(private_chef_path)
  private_chef_rb_not_symlink = !File.symlink?(private_chef_path)
  chef_server_rb_exists = File.exists?(chef_server_path)
  if chef_server_rb_exists
    chef_server_rb_not_empty = !File.zero?(chef_server_path)
  end
  # Things are a bit tricky here, because there are multiple scenarios.  But the
  # upshot is, if we have a private-chef.rb and don't have (or have an empty)
  # chef-server.rb, then copy it over and link back.  Otherwise warn.
  if private_chef_rb_exists && private_chef_rb_not_symlink && chef_server_rb_exists &&
      chef_server_rb_not_empty
    Chef::Log.warn("/etc/opscode/private-chef.rb is deprecated and should be removed. Using /etc/opscode/chef-server.rb")
  elsif private_chef_rb_exists && private_chef_rb_not_symlink
    Chef::Log.warn("Moving to /etc/opscode/chef-server.rb for configuration - /etc/opscode/private-chef.rb is deprecated.")
    FileUtils.mv(private_chef_path, chef_server_path)
    FileUtils.ln_s(chef_server_path, private_chef_path)
    chef_server_rb_exists = true
  end
  if chef_server_rb_exists
    PrivateChef.from_file(chef_server_path)
  end
  node.consume_attributes(PrivateChef.generate_config(node['fqdn']))
end

# Warn about deprecated opscode_webui settings
opscode_webui_deprecation_notice = OpscodeWebuiDeprecationNotice.new(
  PrivateChef['opscode_webui']
)
log 'opscode_webui deprecation notice' do
  message opscode_webui_deprecation_notice.message
  level :warn
  only_if { opscode_webui_deprecation_notice.applicable? }
end

# @todo: This seems like it might belong in the PrivateChef helper;
#   many other attributes like are set automatically there as well.
if OmnibusHelper.has_been_bootstrapped?
  node.set['private_chef']['bootstrap']['enable'] = false
end

# Create the Chef User
include_recipe "private-chef::users"

# merge xdarklaunch values into the disk-based darklaunch
# so that we have a single source of truth for xdl-related
# values
darklaunch_values = node['private_chef']['dark_launch']
  .merge(node['private_chef']['lb']['xdl_defaults'])
  .to_hash

file "/etc/opscode/dark_launch_features.json" do
  owner OmnibusHelper.new(node).ownership['owner']
  group "root"
  mode "0644"
  content Chef::JSONCompat.to_json_pretty(darklaunch_values)
end

webui_key = OpenSSL::PKey::RSA.generate(2048) unless File.exists?('/etc/opscode/webui_pub.pem')

file "/etc/opscode/webui_pub.pem" do
  owner "root"
  group "root"
  mode "0644"
  content webui_key.public_key.to_s unless File.exists?('/etc/opscode/webui_pub.pem')
end

file "/etc/opscode/webui_priv.pem" do
  owner OmnibusHelper.new(node).ownership['owner']
  group "root"
  mode "0600"
  content webui_key.to_pem.to_s unless File.exists?('/etc/opscode/webui_pub.pem')
end

worker_key = OpenSSL::PKey::RSA.generate(2048) unless File.exists?('/etc/opscode/worker-public.pem')

file "/etc/opscode/worker-public.pem" do
  owner "root"
  group "root"
  mode "0644"
  content worker_key.public_key.to_s unless File.exists?('/etc/opscode/worker-public.pem')
end

file "/etc/opscode/worker-private.pem" do
  owner OmnibusHelper.new(node).ownership['owner']
  group "root"
  mode "0600"
  content worker_key.to_pem.to_s unless File.exists?('/etc/opscode/worker-public.pem')
end

unless File.exists?('/etc/opscode/pivotal.pem')
  cert, key = OmnibusHelper.gen_certificate
end

file "/etc/opscode/pivotal.cert" do
  owner "root"
  group "root"
  mode "0644"
  content cert.to_s unless File.exists?('/etc/opscode/pivotal.pem')
end

file "/etc/opscode/pivotal.pem" do
  owner OmnibusHelper.new(node).ownership['owner']
  group "root"
  mode "0600"
  content key.to_pem.to_s unless File.exists?('/etc/opscode/pivotal.pem')
end

directory "/etc/chef" do
  owner "root"
  group OmnibusHelper.new(node).ownership['group']
  mode "0775"
  action :create
end

directory "/var/opt/opscode" do
  owner "root"
  group "root"
  mode "0755"
  recursive true
  action :create
end

directory "/var/log/opscode" do
  owner OmnibusHelper.new(node).ownership['owner']
  group OmnibusHelper.new(node).ownership['group']
  mode "0755"
  action :create
end

# Put keepalived into a safe state before proceeding with
# the opscode-runsvdir -> opscode-private-chef transition
private_chef_keepalived_safemode 'warmfuzzy' do
  only_if { ha? }
  only_if { is_data_master? }
  only_if 'initctl status opscode-runsvdir | grep start'
end

include_recipe "enterprise::runit"
include_recipe "private-chef::sysctl-updates"
# Run plugins first, mostly for ha
include_recipe "private-chef::plugins"
# Configure Services
[
  "rabbitmq",
  "postgresql",
  "oc_bifrost",
  "oc_id",
  "opscode-solr4",
  "opscode-expander",
  "bookshelf",
  "opscode-erchef",
  "bootstrap",
  "opscode-chef-mover",
  "redis_lb",
  "nginx",
  "keepalived"
].each do |service|
  if node["private_chef"][service]["enable"]
    include_recipe "private-chef::#{service}"
  else
    # All non-enabled services get disabled;
    # opscode-expander gets additional special treatment
    #
    # bootstrap isn't really a service, though, so there's
    # nothing to disable, really.
    unless service == 'bootstrap'

      runit_service service do
        action :disable
      end

      case service
      when "opscode-expander"
        runit_service "opscode-expander-reindexer" do
          action :disable
        end
      else
        # nothing to see, move along
      end

    end # unless

  end
end



include_recipe "private-chef::actions" if darklaunch_values["actions"]

include_recipe "private-chef::private-chef-sh"
include_recipe "private-chef::oc-chef-pedant"
include_recipe "private-chef::log_cleanup"
include_recipe "private-chef::partybus"
include_recipe "private-chef::ctl_config"
include_recipe "private-chef::disable_chef_server_11"

file "/etc/opscode/chef-server-running.json" do
  owner OmnibusHelper.new(node).ownership['owner']
  group "root"
  mode "0600"

  file_content = {
    "private_chef" => node['private_chef'].to_hash,
    "run_list" => node.run_list,
    "runit" => node['runit'].to_hash
  }
  # back-compat fixes for opscode-reporting
  # reporting uses the opscode-solr key for determining the location of the solr host,
  # so we'll copy the contents over from opscode-solr4
  file_content['private_chef']['opscode-solr'] ||= {}
  %w{vip port}.each do |key|
    file_content['private_chef']['opscode-solr'][key] = file_content['private_chef']['opscode-solr4'][key]
  end

  content Chef::JSONCompat.to_json_pretty(file_content)
end
