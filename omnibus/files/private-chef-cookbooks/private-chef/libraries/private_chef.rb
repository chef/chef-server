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

  # options are 'ipv4' 'ipv6'
  ip_version "ipv4"

  # Set this for default org mode
  default_orgname nil

  addons Mash.new
  rabbitmq Mash.new
  external_rabbitmq Mash.new
  rabbitmq['log_rotation'] ||= Mash.new
  opscode_solr4 Mash.new
  opscode_solr4['log_rotation'] ||= Mash.new
  opscode_expander Mash.new
  opscode_expander['log_rotation'] ||= Mash.new
  opscode_erchef Mash.new
  opscode_erchef['log_rotation'] ||= Mash.new
  oc_chef_authz Mash.new
  # Need old path for cookbook migration:
  opscode_chef Mash.new

  lb Mash.new
  lb['xdl_defaults'] ||= Mash.new
  lb_internal Mash.new
  postgresql Mash.new
  postgresql['log_rotation'] ||= Mash.new
  redis_lb Mash.new
  redis_lb['log_rotation'] ||= Mash.new
  oc_bifrost Mash.new
  oc_bifrost['log_rotation'] ||= Mash.new
  oc_id Mash.new
  oc_id['log_rotation'] ||= Mash.new
  bookshelf Mash.new
  bookshelf['log_rotation'] ||= Mash.new
  bootstrap Mash.new
  drbd Mash.new # For DRBD specific settings
  keepalived Mash.new
  estatsd Mash.new
  nginx Mash.new
  nginx['log_rotation'] ||= Mash.new
  log_retention Mash.new
  log_rotation Mash.new
  dark_launch Mash.new
  opscode_chef_mover Mash.new
  oc_chef_pedant Mash.new
  insights Mash.new

  license Mash.new

  folsom_graphite Mash.new

  servers Mash.new
  backend_vips Mash.new
  ha Mash.new # For all other HA settings
  api_fqdn nil
  node nil

  notification_email nil
  from_email nil
  role nil
  user Mash.new

  ldap Mash.new
  disabled_plugins []
  enabled_plugins []

  backup Mash.new
  backup['strategy'] = 'tar'

  # - legacy config mashes -
  # these config values are here so that if any config has been previously
  # set for these projects in an older version of private-chef/chef-server.rb
  # then we do not error out during the reconfigure
  opscode_webui Mash.new
  opscode_solr Mash.new
  couchdb Mash.new
  opscode_account Mash.new
  opscode_org_creator Mash.new
  opscode_certificate Mash.new

  registered_extensions Mash.new

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

    def import_legacy_service_config(old_service_key, new_service_key, keys)
      keys.each do |configkey|
        if PrivateChef[old_service_key].has_key? configkey
          PrivateChef[new_service_key][configkey] ||= PrivateChef[old_service_key][configkey]
          PrivateChef[old_service_key].delete configkey
        end
      end
    end

    VALID_EXTENSION_CONFIGS= %i(server_config_required config_values gen_backend gen_frontend gen_secrets gen_api_fqdn) unless defined?(VALID_EXTENSION_CONFIGS)
    def register_extension(name, extension)
      bad_keys = extension.keys - VALID_EXTENSION_CONFIGS

      bad_keys.each do |key|
        Chef::Log.warn("Extension #{name} contains unknown configuration option: #{key}")
        extension.delete(key) # delete mutates extension
      end

      PrivateChef["registered_extensions"][name] = extension
      if extension.key?(:config_values)
        extension[:config_values].each do |k, v|
          # TODO(ssd): Should we even allow this? Perhaps we should just exit?
          if PrivateChef.has_key?(k)
            Chef::Log.warn("Extension #{name} attempted to register configuration default for #{k}, but it already exists!")
          end
          PrivateChef[k] = v
        end
      end
    end

    def server(name=nil, opts={})
      if name
        PrivateChef["servers"] ||= Mash.new
        PrivateChef["servers"][name] = Mash.new(opts)
      end
      PrivateChef["servers"]
    end

    def backend_vip(name=nil, opts={})
      if name
        PrivateChef['backend_vips'] ||= Mash.new
        PrivateChef['backend_vips']["fqdn"] = name
        opts.each do |k,v|
          PrivateChef['backend_vips'][k] = v
        end

        # Keepalived needs an address with network to properly configure an IPv6 vip
        if PrivateChef['backend_vips']['ipaddress'] =~ /(.*)\/(\d+)/
          # If we have an address of the form addr/mask, split it out
          PrivateChef['backend_vips']['ipaddress'] = $1
          PrivateChef['backend_vips']['ipaddress_with_netmask'] = "#{$1}/#{$2}"
        elsif PrivateChef['backend_vips']['ipaddress'] =~ /\:/
          # IPv6 addresses must have the mask
          Chef::Log.fatal("backend_vip ipaddress field appears to be a IPv6 address without a netmask  (e.g /64, /48)")
          exit 66
        else
          # bare addresses (IPv4) can not have a mask (to preserve backwards compatibility)
          PrivateChef['backend_vips']['ipaddress_with_netmask'] = PrivateChef['backend_vips']['ipaddress']
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

    def generate_hash
      results = { "private_chef" => {} }
      default_keys = [
        "opscode_chef",
        "redis_lb",
        "addons",
        "rabbitmq",
        "external_rabbitmq",
        "opscode_solr4",
        "opscode_expander",
        "opscode_erchef",
        "oc_chef_authz",
        "folsom_graphite",
        "lb",
        "lb_internal",
        "postgresql",
        "oc_bifrost",
        "oc_id",
        "opscode_chef_mover",
        "bookshelf",
        "bootstrap",
        "drbd",
        "keepalived",
        "estatsd",
        "nginx",
        "ldap",
        "user",
        "ha",
        "disabled_plugins",
        "enabled_plugins",
        "license",
        "backup",
        "insights",
        # keys for cleanup and back-compat
        "couchdb",
        "opscode_solr"]

      (default_keys | keys_from_extensions).each do |key|
        # @todo: Just pick a naming convention and adhere to it
        # consistently
        rkey = if key =~ /^oc_/ || key == "redis_lb"
                 key # leave oc_* keys as is
               else
                 key.gsub('_', '-')
               end
        results['private_chef'][rkey] = PrivateChef[key]
      end
      results['private_chef']['default_orgname'] = PrivateChef['default_orgname']
      results['private_chef']['oc-chef-pedant'] = PrivateChef['oc_chef_pedant']
      results['private_chef']['notification_email'] = PrivateChef['notification_email']
      results['private_chef']['from_email'] = PrivateChef['from_email']
      results['private_chef']['role'] = PrivateChef['role']
      results['private_chef']['topology'] = PrivateChef['topology']
      results['private_chef']['servers'] = PrivateChef['servers']
      results['private_chef']['backend_vips'] = PrivateChef['backend_vips']
      results['private_chef']['logs'] = {}
      results['private_chef']['logs']['log_retention'] = PrivateChef['log_retention']
      results['private_chef']['logs']['log_rotation'] = PrivateChef['log_rotation']
      results['private_chef']['dark_launch'] = PrivateChef['dark_launch']
      results['private_chef']['opscode-erchef']['max_request_size'] = PrivateChef["opscode_erchef"]["max_request_size"]
      results['private_chef']['folsom_graphite'] = PrivateChef['folsom_graphite']
      results
    end

    #
    # Returns an array of the configuration keys added by registered
    # extensions. These keys should be rendered to the final
    # configuration
    #
    def keys_from_extensions
      PrivateChef["registered_extensions"].map do |name, ext|
        if ext[:config_values]
          ext[:config_values].keys.map {|k| k.to_s}
        end
      end.flatten.compact
    end

    def gen_hapaths
      PrivateChef["ha"]["provider"] ||= "drbd" # Use drbd by default for HA
      PrivateChef["enabled_plugins"] << "chef-ha-#{PrivateChef["ha"]["provider"]}"
      PrivateChef["ha"]["path"] ||= "/var/opt/opscode/drbd/data"
      hapath = PrivateChef["ha"]["path"]
      PrivateChef['bookshelf']['data_dir'] = "#{hapath}/bookshelf"
      PrivateChef["rabbitmq"]["data_dir"] ||= "#{hapath}/rabbitmq"
      PrivateChef["opscode_solr4"]["data_dir"] ||= "#{hapath}/opscode-solr4"
      PrivateChef["redis_lb"]["data_dir"] ||= "#{hapath}/redis_lb"

      # cleanup back-compat
      # in order to delete the data directories for opscode-solr and couchdb
      # after installing Chef Server 12, we need to have access to the configuration
      # data as if it were configured for Enterprise Chef 11.
      PrivateChef['couchdb']['data_dir'] = "#{hapath}/couchdb"
      PrivateChef['opscode_solr']['data_dir'] = "#{hapath}/opscode-solr"

      # The postgresql data directory is scoped to the current version;
      # changes in the directory trigger upgrades from an old PostgreSQL
      # version to a newer one
      PrivateChef["postgresql"]["data_dir"] ||= "#{hapath}/postgresql_#{node['private_chef']['postgresql']['version']}"

      # Need old path for cookbook migration
      PrivateChef['opscode_chef']['checksum_path'] ||= "#{hapath}/opscode-chef/checksum"
    end

    def gen_keepalived(node_name)
      PrivateChef['servers'].each do |k, v|
        next unless v['role'] == "backend"
        next if k == node_name
        PrivateChef['servers'][node_name]['peer_ipaddress'] = v['ipaddress']
      end
      PrivateChef["keepalived"]["enable"] ||= true
      PrivateChef["keepalived"]["ipv6_on"] ||= PrivateChef["use_ipv6"]
      PrivateChef["keepalived"]["vrrp_instance_interface"] = backend_vip["device"]
      PrivateChef["keepalived"]["vrrp_instance_ipaddress"] = backend_vip["ipaddress_with_netmask"]
      PrivateChef["keepalived"]["vrrp_instance_ipaddress_dev"] = backend_vip["device"]
      PrivateChef["keepalived"]["vrrp_instance_vrrp_unicast_bind"] = PrivateChef['servers'][node_name]['ipaddress']
      PrivateChef["keepalived"]["vrrp_instance_vrrp_unicast_peer"] = PrivateChef['servers'][node_name]['peer_ipaddress']
      PrivateChef["keepalived"]["vrrp_instance_ipaddress_dev"] = backend_vip["device"]
      PrivateChef["bookshelf"]["ha"] ||= true
      PrivateChef["oc_id"]["ha"] ||= true
      PrivateChef["rabbitmq"]["ha"] ||= true
      PrivateChef["opscode_solr4"]["ha"] ||= true
      PrivateChef["opscode_expander"]["ha"] ||= true
      PrivateChef["opscode_erchef"]["ha"] ||= true
      PrivateChef["lb"]["ha"] ||= true
      PrivateChef["postgresql"]["ha"] ||= true
      PrivateChef["redis_lb"]["ha"] ||= true
      PrivateChef["oc_bifrost"]["ha"] ||= true
      PrivateChef["nginx"]["ha"] ||= true
    end

    #
    # Genereric gen_ callbacks
    #
    # - gen_frontend: Run on all frontend nodes
    #
    # - gen_backend: Runs on all backend nodes, take a parameter
    # indicating whether the current node is the bootstrap node.
    #
    # - gen_api_fqdn: Runs on all nodes
    #
    # - gen_secrets: Runs on all nodes but should abort if a needed
    # secret doesn't exist on a non-bootstrap node. Takes the node
    # name.
    #
    # If a plugin doesn't define a callback for one of these functions
    # the generic version is used.
    #
    def gen_frontend
      callback = callback_for(:gen_frontend)
      if ! callback.nil?
        instance_exec(&callback)
      else
        gen_frontend_default
      end
    end

    def gen_backend(bootstrap=false)
      callback = callback_for(:gen_backend)
      if ! callback.nil?
        instance_exec(bootstrap, &callback)
      else
        gen_backend_default(bootstrap)
      end
    end

    def gen_secrets(node_name)
      callback = callback_for(:gen_secrets)
      if ! callback.nil?
        instance_exec(node_name, &callback)
      else
        gen_secrets_default(node_name)
      end
    end

    def gen_api_fqdn
      callback = callback_for(:gen_api_fqdn)
      if ! callback.nil?
        instance_exec(&callback)
      else
        gen_api_fqdn_default
      end
    end

    def callback_for(name)
      extension = PrivateChef["registered_extensions"][PrivateChef["topology"]]
      if extension
        extension[name]
      else
        nil
      end
    end

    #
    # Default implementation of gen_ callbacks
    #
    # Currently these are focused on providing the tier & HA toplogies
    # but will do less and less as that functionality moves into the
    # plugin
    #
    def gen_backend_default(bootstrap)
      PrivateChef[:role] = "backend" #mixlib-config wants a symbol :(
      PrivateChef["bookshelf"]["listen"] ||= PrivateChef["default_listen_address"]
      PrivateChef["rabbitmq"]["node_ip_address"] ||= PrivateChef["default_listen_address"]
      PrivateChef["redis_lb"]["listen"] ||= PrivateChef["default_listen_address"]
      PrivateChef["opscode_solr4"]["ip_address"] ||= PrivateChef["default_listen_address"]
      PrivateChef["postgresql"]["listen_address"] ||= '*' #PrivateChef["default_listen_address"]

      authaddr = []
      authaddr << "0.0.0.0/0" # if PrivateChef["use_ipv4"]
      authaddr << "::/0" if PrivateChef["use_ipv6"]
      PrivateChef["postgresql"]["md5_auth_cidr_addresses"] ||= authaddr

      PrivateChef["opscode_chef_mover"]["enable"] = !!bootstrap
      PrivateChef["bootstrap"]["enable"] = !!bootstrap
    end

    def gen_frontend_default
      PrivateChef[:role] = "frontend"
      PrivateChef["bookshelf"]["enable"] ||= false
      PrivateChef["bookshelf"]["vip"] ||= PrivateChef["backend_vips"]["ipaddress"]
      PrivateChef["rabbitmq"]["enable"] ||= false
      PrivateChef["rabbitmq"]["vip"] ||= PrivateChef["backend_vips"]["ipaddress"]
      PrivateChef["redis_lb"]["enable"] ||= false
      PrivateChef["redis_lb"]["vip"] ||= PrivateChef["backend_vips"]["ipaddress"]
      PrivateChef["opscode_solr4"]["enable"] ||= false
      PrivateChef["opscode_solr4"]["vip"] ||= PrivateChef["backend_vips"]["ipaddress"]
      PrivateChef["opscode_expander"]["enable"] ||= false
      PrivateChef["postgresql"]["enable"] ||= false
      PrivateChef["postgresql"]["vip"] ||= PrivateChef["backend_vips"]["ipaddress"]
      PrivateChef["lb"]["cache_cookbook_files"] ||= true
      PrivateChef["lb"]["upstream"] ||= Mash.new
      if PrivateChef["use_ipv6"] && PrivateChef["backend_vips"]["ipaddress"].include?(':')
        PrivateChef["lb"]["upstream"]["bookshelf"] ||= [ "[#{PrivateChef["backend_vips"]["ipaddress"]}]" ]
      else
        PrivateChef["lb"]["upstream"]["bookshelf"] ||= [ PrivateChef["backend_vips"]["ipaddress"] ]
      end
      PrivateChef["opscode_chef_mover"]["enable"] = false
      PrivateChef["bootstrap"]["enable"] = false
    end

    def gen_api_fqdn_default
      PrivateChef["lb"]["api_fqdn"] ||= PrivateChef['api_fqdn']
      PrivateChef["lb"]["web_ui_fqdn"] ||= PrivateChef['api_fqdn']
      PrivateChef["nginx"]["server_name"] ||= PrivateChef['api_fqdn']
      PrivateChef["nginx"]["url"] ||= "https://#{PrivateChef['api_fqdn']}"
    end

    def gen_secrets_default(node_name)
      consume_existing_secrets
      # Transition from erchef's sql_user/password etc living under 'postgresql' in older versions,
      # to 'opscode_erchef' in newer versions.
      if PrivateChef['postgresql'].has_key? 'sql_password'
        PrivateChef['opscode_erchef']['sql_password'] ||= PrivateChef['postgresql']['sql_password']
        PrivateChef['postgresql'].delete 'sql_password'
        PrivateChef['postgresql'].delete 'sql_user'
      end
      if PrivateChef['postgresql'].has_key? 'sql_ro_password'
        PrivateChef['opscode_erchef']['sql_ro_password'] ||= PrivateChef['postgresql']['sql_ro_password']
        PrivateChef['postgresql'].delete 'sql_ro_password'
        PrivateChef['postgresql'].delete 'sql_ro_user'
      end

      me = PrivateChef["servers"][node_name]
      ha_guard = PrivateChef['topology'] == 'ha' && !me['bootstrap']

      PrivateChef['postgresql']['db_superuser_password'] ||= generate_hex_if_bootstrap(50, ha_guard)
      PrivateChef['redis_lb']['password'] ||= generate_hex_if_bootstrap(50, ha_guard)
      PrivateChef['rabbitmq']['password'] ||= generate_hex_if_bootstrap(50, ha_guard)
      PrivateChef['rabbitmq']['actions_password'] ||= generate_hex_if_bootstrap(50, ha_guard)
      PrivateChef['rabbitmq']['management_password'] ||= generate_hex_if_bootstrap(50, ha_guard)
      PrivateChef['drbd']['shared_secret'] ||= generate_hex_if_bootstrap(30, ha_guard)
      PrivateChef['keepalived']['vrrp_instance_password'] ||= generate_hex_if_bootstrap(50, ha_guard)
      PrivateChef['opscode_erchef']['sql_password'] ||= generate_hex_if_bootstrap(30, ha_guard)
      PrivateChef['opscode_erchef']['sql_ro_password'] ||= generate_hex_if_bootstrap(30, ha_guard)
      PrivateChef['oc_bifrost']['superuser_id'] ||= generate_hex_if_bootstrap(16, ha_guard)
      PrivateChef['oc_bifrost']['sql_password'] ||= generate_hex_if_bootstrap(50, ha_guard)
      PrivateChef['oc_bifrost']['sql_ro_password'] ||= generate_hex_if_bootstrap(50, ha_guard)
      PrivateChef['oc_id']['secret_key_base'] ||= generate_hex_if_bootstrap(50, ha_guard)
      PrivateChef['oc_id']['sql_password'] ||= generate_hex_if_bootstrap(50, ha_guard)
      PrivateChef['oc_id']['sql_ro_password'] ||= generate_hex_if_bootstrap(50, ha_guard)
      PrivateChef['bookshelf']['access_key_id'] ||= generate_hex_if_bootstrap(20, ha_guard)
      PrivateChef['bookshelf']['secret_access_key'] ||= generate_hex_if_bootstrap(40, ha_guard)
      PrivateChef['bookshelf']['sql_password'] ||= generate_hex_if_bootstrap(40, ha_guard)
      PrivateChef['bookshelf']['sql_ro_password'] ||= generate_hex_if_bootstrap(40, ha_guard)
    end

    def gen_redundant(node_name, topology)
      me = PrivateChef["servers"][node_name]
      case me["role"]
      when "backend"
        gen_backend(me['bootstrap'])
        # TODO(ssd): Move these into gen_backend callback for HA
        # plugin once that is split out
        gen_hapaths if topology == 'ha'
        gen_keepalived(node_name) if topology == 'ha'
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
      # DEPRECATION ALERT
      # Under chef 11, there were two ways to enable encryption. First you could
      # just set ssl_enabled and the proper port.  Second, you could set the proper port,
      # and set 'encryption' to simple_tls.  We also documented accepting 'start_tls'.
      # If you set encryption and used ssl_enabled, it was effectively the same as setting
      # encryption to simple_tls
      #
      #
      # For compatibility, we will accept both methods here, but in the future valid options are
      # to use either 'ssl_enabled' or 'tls_enabled'. The ability to directly set 'encryption' is
      # deprecated.
      #
      ldap_encryption = PrivateChef['ldap']['encryption']
      ssl_enabled = PrivateChef['ldap']['ssl_enabled']
      tls_enabled = PrivateChef['ldap']['tls_enabled']

      # Some edge case checks, tell them if they set more than one value and which one we're using.
      # This can go away once ldap_encryption support is removed
      if ldap_encryption
        if ssl_enabled
          Chef::Log.warn("Both ldap['encryption'] and ldap['ssl_enabled'] are set. Using ssl_enabled.")
          ldap_encryption = nil
        elsif tls_enabled
          Chef::Log.warn("Both ldap['encryption'] and ldap['tls_enabled'] are set. Using tls_enabled.")
          ldap_encryption = nil
        end
      end
      if ldap_encryption
        Chef::Log.warn("Please note that the ldap 'encryption' setting is deprecated as of Chef Server 12.0. Use either "\
                       "ldap['ssl_enabled'] = true or ldap['tls_enabled'] = true.")
        case ldap_encryption.to_s
        when "simple_tls"
          ssl_enabled = true
        when "start_tls"
          tls_enabled = true
        when "none"
          Chef::Log.info("Configuring ldap without encryption.")
        else
          raise "Invalid ldap configuration: unknown value #{ldap_encryption} for deprecated ldap['encryption'] option. "\
                "Please set ldap['ssl_enabled'] = true or ldap['tls_enabled'] = true instead"
        end
      else
        if ssl_enabled and tls_enabled
          raise "Invalid ldap configuration: ldap['ssl_enabled'] and ldap['tls_enabled'] are mutually exclusive."
        end
      end
      PrivateChef["ldap"]["ssl_enabled"] = ssl_enabled
      PrivateChef["ldap"]["tls_enabled"] = tls_enabled
      PrivateChef["ldap"]["encryption_type"] = ssl_enabled ? "simple_tls" :
                                               tls_enabled ? "start_tls" :
                                               "none"
    end

    # True if the given topology requires per-server config via `server` blocks
    def server_config_required?
      PrivateChef["topology"] == "ha" ||
        PrivateChef["topology"] == "tier" ||
        (PrivateChef["registered_extensions"][PrivateChef["topology"]] &&
         PrivateChef["registered_extensions"][PrivateChef["topology"]][:server_config_required])
    end

    def assert_server_config(node_name)
      unless PrivateChef["servers"].key?(node_name)
        Chef::Log.fatal <<-EOF
No server configuration found for "#{node_name}" in /etc/opscode/chef-server.rb.
Server configuration exists for the following hostnames:

  #{PrivateChef["servers"].keys.sort.join("\n  ")}

EOF
        exit!(1)
      end
    end

    def generate_config_for_topology(topology, node_name)
      # TODO(ssd): This can be cleaned up once the "default"
      # topologies are also implemented using registered extensions
      case topology
      when "standalone","manual"
        PrivateChef[:api_fqdn] ||= node_name
        gen_api_fqdn
      when "ha", "tier"
        gen_redundant(node_name, topology)
      else
        if PrivateChef["registered_extensions"].key?(topology) && server_config_required?
          gen_redundant(node_name, topology)
        elsif PrivateChef["registered_extensions"].key?(topology) && !server_config_required?
          PrivateChef[:api_fqdn] ||= node_name
          gen_api_fqdn
        else
          Chef::Log.fatal("I do not understand topology #{PrivateChef.topology} - try standalone, manual, ha, or tier.")
          exit 55
        end
      end
    end

    def generate_config(node_name)
      assert_server_config(node_name) if server_config_required?
      gen_secrets(node_name)
      write_secrets_file

      # Under ipv4 default to 0.0.0.0 in order to ensure that
      # any service that needs to listen externally on back-end
      # does so.
      PrivateChef["default_listen_address"] = "0.0.0.0"
      # 'ipv4, ipv6, maybe add both
      case PrivateChef['ip_version']
      when 'ipv4', nil
        PrivateChef["use_ipv4"] = true
        PrivateChef["use_ipv6"] = false
        PrivateChef["default_listen_address"] = "0.0.0.0"
      when 'ipv6'
        PrivateChef["use_ipv4"] = false
        PrivateChef["use_ipv6"] = true
        PrivateChef["default_listen_address"] = "::"
      end

      # Transition Solr memory and JVM settings from OSC11 to Chef 12.
      import_legacy_service_config('opscode_solr', 'opscode_solr4', ['heap_size', 'new_size', 'java_opts'])

      PrivateChef["nginx"]["enable_ipv6"] ||= PrivateChef["use_ipv6"]

      generate_config_for_topology(PrivateChef["topology"], node_name)

      unless PrivateChef["ldap"].nil? || PrivateChef["ldap"].empty?
        gen_ldap
      end

      generate_hash
    end

    def existing_secrets
      @existing_secrets ||= if File.exists?("/etc/opscode/private-chef-secrets.json")
                              Chef::JSONCompat.from_json(File.read("/etc/opscode/private-chef-secrets.json"))
                            else
                              {}
                            end
    end

    def consume_existing_secrets
      existing_secrets.each do |k, v|
        v.each do |pk, p|
          if not PrivateChef[k]
            Chef::Log.info("Ignoring unused secret for #{k}.")
          else
            PrivateChef[k][pk] = p
          end
        end
      end
    end

    def write_secrets_file
      if File.directory?("/etc/opscode")
        # This was originally directly written via f.puts(Chef::JSONCompat.to_json_pretty)
        # Let's instead assemble this hash externally so that if it fails for any reason
        # we don't wipe out the secrets file.
        out_json = Chef::JSONCompat.to_json_pretty({
          'redis_lb' => {
            'password' => PrivateChef['redis_lb']['password']
          },
          'rabbitmq' => {
            'password' => PrivateChef['rabbitmq']['password'],
            'actions_password' => PrivateChef['rabbitmq']['actions_password'],
            'management_password' => PrivateChef['rabbitmq']['management_password']
          },
          'postgresql' => {
            'db_superuser_password' => PrivateChef['postgresql']['db_superuser_password']
          },
          'opscode_erchef' => {
            'sql_password' => PrivateChef['opscode_erchef']['sql_password'],
            'sql_ro_password' => PrivateChef['opscode_erchef']['sql_ro_password']
          },
          'oc_id' => {
            'sql_password' => PrivateChef['oc_id']['sql_password'],
            'sql_ro_password' => PrivateChef['oc_id']['sql_ro_password'],
            'secret_key_base' => PrivateChef['oc_id']['secret_key_base']
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
            'sql_password' => PrivateChef['bookshelf']['sql_password'],
            'sql_ro_password' => PrivateChef['bookshelf']['sql_ro_password'],
            'access_key_id' => PrivateChef['bookshelf']['access_key_id'],
            'secret_access_key' => PrivateChef['bookshelf']['secret_access_key']
          }})

        File.open("/etc/opscode/private-chef-secrets.json", "w") do |f|
          f.puts(out_json)
          system("chmod 0600 /etc/opscode/private-chef-secrets.json")
        end
      end
    end
  end
end
