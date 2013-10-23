#
# Author:: Adam Jacob (<adam@opscode.com>)
# Copyright:: Copyright (c) 2012 Opscode, Inc.
#

require 'uuidtools'
require 'openssl'

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
  Chef::Log.warn("Please move to /etc/opscode/private-chef.rb for configuration - /etc/opscode/chef-server.json is deprecated.")
else
  PrivateChef[:node] = node
  if File.exists?("/etc/opscode/private-chef.rb")
    PrivateChef.from_file("/etc/opscode/private-chef.rb")
  end
  node.consume_attributes(PrivateChef.generate_config(node['fqdn']))
end

# @todo: This seems like it might belong in the PrivateChef helper;
#   many other attributes like are set automatically there as well.
if OmnibusHelper.has_been_bootstrapped?
  node.set['private_chef']['bootstrap']['enable'] = false
end

# Create the Chef User
include_recipe "private-chef::users"

file "/etc/opscode/dark_launch_features.json" do
  owner node["private_chef"]["user"]["username"]
  group "root"
  mode "0644"
  content Chef::JSONCompat.to_json_pretty(node['private_chef']['dark_launch'].to_hash)
end

webui_key = OpenSSL::PKey::RSA.generate(2048) unless File.exists?('/etc/opscode/webui_pub.pem')

file "/etc/opscode/webui_pub.pem" do
  owner "root"
  group "root"
  mode "0644"
  content webui_key.public_key.to_s unless File.exists?('/etc/opscode/webui_pub.pem')
end

file "/etc/opscode/webui_priv.pem" do
  owner node["private_chef"]["user"]["username"]
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
  owner node["private_chef"]["user"]["username"]
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
  owner node["private_chef"]["user"]["username"]
  group "root"
  mode "0600"
  content key.to_pem.to_s unless File.exists?('/etc/opscode/pivotal.pem')
end

directory "/etc/chef" do
  owner "root"
  group node['private_chef']['user']['username']
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

include_recipe "enterprise::runit"

# Configure Services
[
  "drbd",
  "couchdb",
  "rabbitmq",
  "postgresql",
  "oc_bifrost",
  "opscode-certificate",
  "opscode-account",
  "opscode-solr",
  "opscode-expander",
  "bookshelf",
  "opscode-org-creator",
  "opscode-erchef",
  "bootstrap",
  "opscode-webui",
  "opscode-chef-mover",
  "nginx",
  "keepalived"
].each do |service|
  if node["private_chef"][service]["enable"]
    include_recipe "private-chef::#{service}"
  else
    include_recipe "private-chef::#{service}_disable"
  end
end

include_recipe "private-chef::orgmapper"
include_recipe "private-chef::oc-chef-pedant"
include_recipe "private-chef::log_cleanup"
include_recipe "private-chef::partybus"

file "/etc/opscode/chef-server-running.json" do
  owner node["private_chef"]["user"]["username"]
  group "root"
  mode "0600"
  content Chef::JSONCompat.to_json_pretty({ "private_chef" => node['private_chef'].to_hash,
                                            "run_list" => node.run_list,
                                            "runit" => node['runit'].to_hash})
end
