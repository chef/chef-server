#
# Author:: Adam Jacob (<adam@opscode.com>)
# Copyright:: Copyright (c) 2012 Opscode, Inc.
#
# All Rights Reserved
#

require 'mixlib/config'
require 'chef/mash'
require 'chef/json_compat'
require 'chef/mixin/deep_merge'
require 'securerandom'

module PrivateChef
  extend(Mixlib::Config)

  # options are 'standalone', 'manual', 'ha', and 'tier'
  topology "standalone"

  couchdb Mash.new
  rabbitmq Mash.new
  opscode_solr Mash.new
  opscode_expander Mash.new
  opscode_erchef Mash.new
  # Need old path for cookbook migration:
  opscode_chef Mash.new
  opscode_webui Mash.new
  lb Mash.new
  postgresql Mash.new
  oc_bifrost Mash.new
  opscode_certificate Mash.new
  opscode_org_creator Mash.new
  opscode_account Mash.new
  bookshelf Mash.new
  bootstrap Mash.new
  drbd Mash.new
  keepalived Mash.new
  estatsd Mash.new
  nginx Mash.new
  log_retention Mash.new
  log_rotation Mash.new
  dark_launch Mash.new

  servers Mash.new
  backend_vips Mash.new
  api_fqdn nil
  node nil

  notification_email nil
  from_email nil
  role nil
  user Mash.new

  ldap Mash.new

  allowed_webui_subnets []

  class << self

    def from_file(filename)
      # We're overriding this here so that we can get more meaningful errors from
      # the reconfigure chef run; we don't particularly care what line in the chef
      # recipes is failing to evaluate the loaded file (in the case of what
      # originally triggered this, private_chef.rb), what we care about is which
      # line in the loaded file is causing the error.
      begin
        self.instance_eval(IO.read(filename), filename, 1)
      rescue
        raise "Error loading file: #{$!.backtrace[0]}: #{$!.message}"
      end
    end

    def server(name=nil, opts={})
      if name
        PrivateChef["servers"] ||= Mash.new
        PrivateChef["servers"][name] = Mash.new(opts)
      end
      PrivateChef["servers"]
    end

    def servers
      PrivateChef["servers"]
    end

    def backend_vip(name=nil, opts={})
      if name
        PrivateChef['backend_vips'] ||= Mash.new
        PrivateChef['backend_vips']["fqdn"] = name
        opts.each do |k,v|
          PrivateChef['backend_vips'][k] = v
        end
      end
      PrivateChef['backend_vips']
    end

    # guards against creating secrets on non-bootstrap node
    def generate_hex_if_bootstrap(chars, ha_guard)
      if ha_guard
        Chef::Log.fatal("Attempt to create secrets on non-bootstrap node in an H/A topology, please copy /etc/opscode/* around instead.")
        exit 44
      end
      SecureRandom.hex(chars)
    end

    def generate_secrets(node_name)
      existing_secrets ||= Hash.new
      if File.exists?("/etc/opscode/private-chef-secrets.json")
        existing_secrets = Chef::JSONCompat.from_json(File.read("/etc/opscode/private-chef-secrets.json"))
      end
      existing_secrets.each do |k, v|
        v.each do |pk, p|
          if not PrivateChef[k]
            Chef::Log.info("Ignoring unused secret for #{k}.")
          else
            PrivateChef[k][pk] = p
          end
        end
      end

      me = PrivateChef["servers"][node_name]
      ha_guard = PrivateChef['topology'] == "ha" && !me['bootstrap']

      PrivateChef['rabbitmq']['password'] ||= generate_hex_if_bootstrap(50, ha_guard)
      PrivateChef['rabbitmq']['jobs_password'] ||= generate_hex_if_bootstrap(50, ha_guard)
      PrivateChef['opscode_webui']['cookie_secret'] ||= generate_hex_if_bootstrap(50, ha_guard)
      PrivateChef['postgresql']['sql_password'] ||= generate_hex_if_bootstrap(50, ha_guard)
      PrivateChef['postgresql']['sql_ro_password'] ||= generate_hex_if_bootstrap(50, ha_guard)
      PrivateChef['opscode_account']['session_secret_key'] ||= generate_hex_if_bootstrap(50, ha_guard)
      PrivateChef['drbd']['shared_secret'] ||= generate_hex_if_bootstrap(30, ha_guard)
      PrivateChef['keepalived']['vrrp_instance_password'] ||= generate_hex_if_bootstrap(50, ha_guard)
      PrivateChef['oc_bifrost']['superuser_id'] ||= generate_hex_if_bootstrap(16, ha_guard)
      PrivateChef['oc_bifrost']['sql_password'] ||= generate_hex_if_bootstrap(50, ha_guard)
      PrivateChef['oc_bifrost']['sql_ro_password'] ||= generate_hex_if_bootstrap(50, ha_guard)
      PrivateChef['bookshelf']['access_key_id'] ||= generate_hex_if_bootstrap(20, ha_guard)
      PrivateChef['bookshelf']['secret_access_key'] ||= generate_hex_if_bootstrap(40, ha_guard)

      if File.directory?("/etc/opscode")
        File.open("/etc/opscode/private-chef-secrets.json", "w") do |f|
          f.puts(
            Chef::JSONCompat.to_json_pretty({
              'rabbitmq' => {
                'password' => PrivateChef['rabbitmq']['password'],
                'jobs_password' => PrivateChef['rabbitmq']['jobs_password'],
              },
              'opscode_webui' => {
                'cookie_secret' => PrivateChef['opscode_webui']['cookie_secret'],
              },
              'postgresql' => {
                'sql_password' => PrivateChef['postgresql']['sql_password'],
                'sql_ro_password' => PrivateChef['postgresql']['sql_ro_password']
              },
              'opscode_account' => {
                'session_secret_key' => PrivateChef['opscode_account']['session_secret_key']
              },
              'drbd' => {
                'shared_secret' => PrivateChef['drbd']['shared_secret']
              },
              'keepalived' => {
                'vrrp_instance_password' => PrivateChef['keepalived']['vrrp_instance_password']
              },
              'oc_bifrost' => {
                'superuser_id' => PrivateChef['oc_bifrost']['superuser_id'],
                'sql_password' => PrivateChef['oc_bifrost']['sql_password'],
                'sql_ro_password' => PrivateChef['oc_bifrost']['sql_ro_password']
              },
              'bookshelf' => {
                'access_key_id' => PrivateChef['bookshelf']['access_key_id'],
                'secret_access_key' => PrivateChef['bookshelf']['secret_access_key']
              }
            })
          )
          system("chmod 0600 /etc/opscode/private-chef-secrets.json")
        end
      end
    end

    def generate_hash
      results = { "private_chef" => {} }
      [
        "couchdb",
        "rabbitmq",
        "opscode_solr",
        "opscode_expander",
        "opscode_erchef",
        "opscode_webui",
        "lb",
        "postgresql",
        "oc_bifrost",
        "opscode_certificate",
        "opscode_org_creator",
        "opscode_account",
        "bookshelf",
        "bootstrap",
        "drbd",
        "keepalived",
        "estatsd",
        "nginx",
        "ldap",
        "user"
      ].each do |key|
        rkey = key.gsub('_', '-') unless key =~ /^oc_/ # leave oc_* keys as is
        results['private_chef'][rkey] = PrivateChef[key]
      end
      results['private_chef']['notification_email'] = PrivateChef['notification_email']
      results['private_chef']['from_email'] = PrivateChef['from_email']
      results['private_chef']['role'] = PrivateChef['role']
      results['private_chef']['logs'] = {}
      results['private_chef']['logs']['log_retention'] = PrivateChef['log_retention']
      results['private_chef']['logs']['log_rotation'] = PrivateChef['log_rotation']
      results['private_chef']['dark_launch'] = PrivateChef['dark_launch']
      results['private_chef']['opscode-erchef']['max_request_size'] = PrivateChef["opscode_erchef"]["max_request_size"]
      results
    end

    def gen_api_fqdn
      PrivateChef["lb"]["api_fqdn"] ||= PrivateChef['api_fqdn']
      PrivateChef["lb"]["web_ui_fqdn"] ||= PrivateChef['api_fqdn']
      PrivateChef["nginx"]["server_name"] ||= PrivateChef['api_fqdn']
      PrivateChef["nginx"]["url"] ||= "https://#{PrivateChef['api_fqdn']}"
    end

    def gen_drbd
      PrivateChef["couchdb"]["data_dir"] ||= "/var/opt/opscode/drbd/data/couchdb"
      PrivateChef['bookshelf']['data_dir'] = "/var/opt/opscode/drbd/data/bookshelf"
      PrivateChef["rabbitmq"]["data_dir"] ||= "/var/opt/opscode/drbd/data/rabbitmq"
      PrivateChef["opscode_solr"]["data_dir"] ||= "/var/opt/opscode/drbd/data/opscode-solr"
      PrivateChef["postgresql"]["data_dir"] ||= "/var/opt/opscode/drbd/data/postgresql"
      PrivateChef["drbd"]["enable"] ||= true
      # Need old path for cookbook migration
      PrivateChef['opscode_chef']['checksum_path'] ||= "/var/opt/opscode/drbd/data/opscode-chef/checksum"
      drbd_role = "primary"
      PrivateChef['servers'].each do |k, v|
        next unless v['role'] == "backend"
        PrivateChef["drbd"][drbd_role] ||= {
          "fqdn" => k,
          "ip" => v['cluster_ipaddress'] || v["ipaddress"]
        }
        drbd_role = "secondary"
      end
    end

    def gen_keepalived(node_name)
      PrivateChef['servers'].each do |k, v|
        next unless v['role'] == "backend"
        next if k == node_name
        PrivateChef['servers'][node_name]['peer_ipaddress'] = v['ipaddress']
      end
      PrivateChef["keepalived"]["enable"] ||= true
      PrivateChef["keepalived"]["vrrp_instance_interface"] = backend_vip["device"]
      PrivateChef["keepalived"]["vrrp_instance_ipaddress"] = backend_vip["ipaddress"]
      PrivateChef["keepalived"]["vrrp_instance_ipaddress_dev"] = backend_vip["device"]
      PrivateChef["keepalived"]["vrrp_instance_vrrp_unicast_bind"] = PrivateChef['servers'][node_name]['ipaddress']
      PrivateChef["keepalived"]["vrrp_instance_vrrp_unicast_peer"] = PrivateChef['servers'][node_name]['peer_ipaddress']
      PrivateChef["keepalived"]["vrrp_instance_ipaddress_dev"] = backend_vip["device"]
      PrivateChef["bookshelf"]["ha"] ||= true
      PrivateChef["couchdb"]["ha"] ||= true
      PrivateChef["rabbitmq"]["ha"] ||= true
      PrivateChef["opscode_solr"]["ha"] ||= true
      PrivateChef["opscode_expander"]["ha"] ||= true
      PrivateChef["opscode_erchef"]["ha"] ||= true
      PrivateChef["opscode_webui"]["ha"] ||= true
      PrivateChef["lb"]["ha"] ||= true
      PrivateChef["postgresql"]["ha"] ||= true
      PrivateChef["oc_bifrost"]["ha"] ||= true
      PrivateChef["opscode_certificate"]["ha"] ||= true
      PrivateChef["opscode_org_creator"]["ha"] ||= true
      PrivateChef["opscode_account"]["ha"] ||= true
      PrivateChef["nginx"]["ha"] ||= true
    end

    def gen_backend(bootstrap=false)
      PrivateChef[:role] = "backend" #mixlib-config wants a symbol :(
      PrivateChef["bookshelf"]["listen"] ||= "0.0.0.0"
      PrivateChef["couchdb"]["bind_address"] ||= "0.0.0.0"
      PrivateChef["rabbitmq"]["node_ip_address"] ||= "0.0.0.0"
      PrivateChef["opscode_solr"]["ip_address"] ||= "0.0.0.0"
      PrivateChef["opscode_webui"]["worker_processes"] ||= 2
      PrivateChef["postgresql"]["listen_address"] ||= "0.0.0.0"
      PrivateChef["postgresql"]["md5_auth_cidr_addresses"] ||= ["0.0.0.0/0", "::0/0"]
      PrivateChef["opscode_account"]["worker_processes"] ||= 4
      if bootstrap
        PrivateChef["bootstrap"]["enable"] = true
      else
        PrivateChef["bootstrap"]["enable"] = false
      end
    end

    def gen_frontend
      PrivateChef[:role] = "frontend"
      PrivateChef["bookshelf"]["enable"] ||= false
      PrivateChef["bookshelf"]["vip"] ||= PrivateChef["backend_vips"]["ipaddress"]
      PrivateChef["couchdb"]["enable"] ||= false
      PrivateChef["couchdb"]["vip"] ||= PrivateChef["backend_vips"]["ipaddress"]
      PrivateChef["rabbitmq"]["enable"] ||= false
      PrivateChef["rabbitmq"]["vip"] ||= PrivateChef["backend_vips"]["ipaddress"]
      PrivateChef["opscode_solr"]["enable"] ||= false
      PrivateChef["opscode_solr"]["vip"] ||= PrivateChef["backend_vips"]["ipaddress"]
      PrivateChef["opscode_expander"]["enable"] ||= false
      PrivateChef["opscode_org_creator"]["enable"] ||= false
      PrivateChef["postgresql"]["enable"] ||= false
      PrivateChef["postgresql"]["vip"] ||= PrivateChef["backend_vips"]["ipaddress"]
      PrivateChef["lb"]["cache_cookbook_files"] ||= true
      PrivateChef["lb"]["upstream"] = Mash.new
      PrivateChef["lb"]["upstream"]["bookshelf"] ||= [ PrivateChef["backend_vips"]["ipaddress"] ]
      PrivateChef["bootstrap"]["enable"] = false
    end

    def gen_redundant(node_name, ha=false)
      raise "Please add a server section for #{node_name} to /etc/opscode/private-chef.rb!" unless PrivateChef['servers'].has_key?(node_name)
      me = PrivateChef["servers"][node_name]
      case me["role"]
      when "backend"
        gen_backend(me['bootstrap'])
        gen_drbd if ha
        gen_keepalived(node_name) if ha
        gen_api_fqdn
      when "frontend"
        gen_frontend
        gen_api_fqdn
      else
        raise "I dont have a role for you! Use 'backend' or 'frontend'."
      end
    end

    def gen_ldap
      required_ldap_config_values = %w{ host base_dn }
      # ensure a bind password was provided along with the optional bind_dn
      required_ldap_config_values << "bind_password" if PrivateChef["ldap"].key?("bind_dn")
      PrivateChef["ldap"]["system_adjective"] ||= 'AD/LDAP'
      required_ldap_config_values.each do |val|
        unless PrivateChef["ldap"].key?(val)
          # ensure all values have been set
          raise "Missing required LDAP config value '#{val}'. Required values include [#{required_ldap_config_values.join(', ')}]"
        end
      end
    end

    def generate_config(node_name)
      generate_secrets(node_name)

      case PrivateChef['topology']
      when "standalone","manual"
        PrivateChef[:api_fqdn] ||= node_name
        gen_api_fqdn
      when "ha","tier"
        if PrivateChef['topology'] == "ha"
          gen_redundant(node_name, true)
        else
          gen_redundant(node_name, false)
        end
      else
        Chef::Log.fatal("I do not understand topology #{PrivateChef.topology} - try standalone, manual, ha or tier.")
        exit 55
      end

      unless PrivateChef["ldap"].nil? || PrivateChef["ldap"].empty?
        gen_ldap
      end

      generate_hash
    end
  end
end
