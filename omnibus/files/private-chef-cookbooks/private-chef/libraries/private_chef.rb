#
# Author:: Adam Jacob (<adam@chef.io>)
# Copyright:: 2012-2018 Chef Software, Inc.
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

require 'mixlib/config'
require 'chef/mash'
require 'chef/json_compat'
require 'chef/mixin/deep_merge'
require 'veil'
require_relative './warnings.rb'

module PrivateChef
  extend(Mixlib::Config)

  # options are 'standalone', 'manual', 'tier'
  topology 'standalone'

  # options are 'ipv4' 'ipv6'
  ip_version 'ipv4'

  # Set this for default org mode
  default_orgname nil

  use_chef_backend false
  chef_backend_members []

  addons Mash.new
  elasticsearch Mash.new
  opscode_erchef Mash.new
  opscode_erchef['log_rotation'] ||= Mash.new
  oc_chef_authz Mash.new
  # Need old path for cookbook migration:
  opscode_chef Mash.new

  lb Mash.new
  lb['xdl_defaults'] ||= Mash.new
  lb_internal Mash.new
  haproxy Mash.new
  haproxy['log_rotation'] ||= Mash.new
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
  estatsd Mash.new
  nginx Mash.new
  nginx['log_rotation'] ||= Mash.new
  log_retention Mash.new
  log_rotation Mash.new
  dark_launch Mash.new
  opscode_chef_mover Mash.new
  oc_chef_pedant Mash.new

  license Mash.new

  folsom_graphite Mash.new
  profiles Mash.new

  servers Mash.new
  backend_vips Mash.new
  api_fqdn nil
  node nil

  notification_email nil
  from_email nil
  role nil
  user Mash.new
  fips nil

  ldap Mash.new

  backup Mash.new
  backup['strategy'] = 'tar'

  data_collector Mash.new
  required_recipe Mash.new

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
  disabled_plugins []
  enabled_plugins []
  rabbitmq Mash.new
  rabbitmq['log_rotation'] ||= Mash.new
  external_rabbitmq Mash.new
  registered_extensions Mash.new
  ha Mash.new # For all other HA settings
  drbd Mash.new # For DRBD specific settings
  deprecated_solr_indexing false
  opscode_expander Mash.new
  opscode_expander['log_rotation'] ||= Mash.new
  opscode_solr4 Mash.new
  opscode_solr4['log_rotation'] ||= Mash.new
  # - end legacy config mashed -

  insecure_addon_compat true

  class << self
    def from_file(filename)
      # We're overriding this here so that we can get more meaningful errors from
      # the reconfigure chef run; we don't particularly care what line in the chef
      # recipes is failing to evaluate the loaded file (in the case of what
      # originally triggered this, private_chef.rb), what we care about is which
      # line in the loaded file is causing the error.

      instance_eval(IO.read(filename), filename, 1)
    rescue
      raise "Error loading file: #{$ERROR_INFO.backtrace[0]}: #{$ERROR_INFO.message}"
    end

    def import_legacy_service_config(old_service_key, new_service_key, keys)
      keys.each do |configkey|
        if PrivateChef[old_service_key].key? configkey
          PrivateChef[new_service_key][configkey] ||= PrivateChef[old_service_key][configkey]
          PrivateChef[old_service_key].delete configkey
        end
      end
    end

    def deprecated_postgresql_settings
      %w(shmmax shmall).each do |setting|
        next unless PrivateChef['postgresql'][setting]

        ChefServer::Warnings.warn <<~EOF
          The configuration parameter postgresql['#{setting}'] is no longer used
          by Chef Server. To limit the amount of shared memory used by
          postgresql use postgresql['shared_buffers'] instead.
        EOF
      end
    end

    # Mutate PrivateChef to account for common cases of user-provided
    # types not being what we want
    def transform_to_consistent_types
      if PrivateChef['bookshelf']['storage_type']
        PrivateChef['bookshelf']['storage_type'] = PrivateChef['bookshelf']['storage_type'].to_s
      end
    end

    def server(name = nil, opts = {})
      if name
        PrivateChef['servers'] ||= Mash.new
        PrivateChef['servers'][name] = Mash.new(opts)
      end
      PrivateChef['servers']
    end

    def backend_vip(name = nil, opts = {})
      if name
        PrivateChef['backend_vips'] ||= Mash.new
        PrivateChef['backend_vips']['fqdn'] = name
        opts.each do |k, v|
          PrivateChef['backend_vips'][k] = v
        end

        # Keepalived needs an address with network to properly configure an IPv6 vip
        if PrivateChef['backend_vips']['ipaddress'] =~ %r{(.*)/(\d+)}
          # If we have an address of the form addr/mask, split it out
          PrivateChef['backend_vips']['ipaddress'] = Regexp.last_match(1)
          PrivateChef['backend_vips']['ipaddress_with_netmask'] = "#{Regexp.last_match(1)}/#{Regexp.last_match(2)}"
        elsif PrivateChef['backend_vips']['ipaddress'] =~ /\:/
          # IPv6 addresses must have the mask
          Chef::Log.fatal('backend_vip ipaddress field appears to be a IPv6 address without a netmask  (e.g /64, /48)')
          exit 66
        else
          # bare addresses (IPv4) can not have a mask (to preserve backwards compatibility)
          PrivateChef['backend_vips']['ipaddress_with_netmask'] = PrivateChef['backend_vips']['ipaddress']
        end
      end
      PrivateChef['backend_vips']
    end

    def generate_hash
      results = { 'private_chef' => {} }
      default_keys = [
        'opscode_chef',
        'redis_lb',
        'addons',
        # ospcode_solr4 required to continue to support old-style
        # external Elasticsearch configuration using the opscode_solr4
        # configuration keys.
        'opscode_solr4',
        'elasticsearch',
        'opscode_erchef',
        'oc_chef_authz',
        'folsom_graphite',
        'profiles',
        'lb',
        'lb_internal',
        'postgresql',
        'oc_bifrost',
        'oc_id',
        'opscode_chef_mover',
        'bookshelf',
        'bootstrap',
        'estatsd',
        'nginx',
        'ldap',
        'user',
        'haproxy',
        'use_chef_backend',
        'chef_backend_members',
        'license',
        'backup',
        'data_collector',
        'required_recipe',

        # keys for cleanup and back-compat
        'couchdb',
        'opscode_solr']

      default_keys.each do |key|
        # @todo: Just pick a naming convention and adhere to it
        # consistently
        rkey = if key =~ /^oc_/ || %w(
          redis_lb
          use_chef_backend
          chef_backend_members
          data_collector
          required_recipe
        ).include?(key)
                 key
               else
                 key.gsub('_', '-')
               end
        results['private_chef'][rkey] = PrivateChef[key]
      end
      #
      # Before Chef Client 15 deep merge of attributes dropped 'nil' even if it was higher precedence.
      #
      set_target_array_if_not_nil(results['private_chef'], 'default_orgname', PrivateChef['default_orgname'])
      set_target_array_if_not_nil(results['private_chef'], 'oc-chef-pedant', PrivateChef['oc_chef_pedant'])
      set_target_array_if_not_nil(results['private_chef'], 'notification_email', PrivateChef['notification_email'])
      set_target_array_if_not_nil(results['private_chef'], 'from_email', PrivateChef['from_email'])
      set_target_array_if_not_nil(results['private_chef'], 'fips_enabled', PrivateChef['fips'])
      set_target_array_if_not_nil(results['private_chef'], 'role', PrivateChef['role'])
      set_target_array_if_not_nil(results['private_chef'], 'topology', PrivateChef['topology'])
      set_target_array_if_not_nil(results['private_chef'], 'servers', PrivateChef['servers'])
      set_target_array_if_not_nil(results['private_chef'], 'backend_vips', PrivateChef['backend_vips'])
      results['private_chef']['logs'] = {}
      set_target_array_if_not_nil(results['private_chef']['logs'], 'log_retention', PrivateChef['log_retention'])
      set_target_array_if_not_nil(results['private_chef'], 'dark_launch', PrivateChef['dark_launch'])
      set_target_array_if_not_nil(results['private_chef']['opscode-erchef'], 'max_request_size', PrivateChef['opscode_erchef']['max_request_size'])
      set_target_array_if_not_nil(results['private_chef'], 'folsom_graphite', PrivateChef['folsom_graphite'])
      set_target_array_if_not_nil(results['private_chef'], 'profiles', PrivateChef['profiles'])
      set_target_array_if_not_nil(results['private_chef'], 'insecure_addon_compat', PrivateChef['insecure_addon_compat'])
      results
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
    #
    def gen_backend(bootstrap)
      PrivateChef[:role] = 'backend' # mixlib-config wants a symbol :(
      PrivateChef['bookshelf']['listen'] ||= PrivateChef['default_listen_address']
      PrivateChef['redis_lb']['listen'] ||= PrivateChef['default_listen_address']
      PrivateChef['elasticsearch']['listen'] ||= PrivateChef['default_listen_address']
      PrivateChef['postgresql']['listen_address'] ||= '*' # PrivateChef["default_listen_address"]

      authaddr = []
      authaddr << '0.0.0.0/0' # if PrivateChef["use_ipv4"]
      authaddr << '::/0' if PrivateChef['use_ipv6']
      PrivateChef['postgresql']['md5_auth_cidr_addresses'] ||= authaddr

      PrivateChef['opscode_chef_mover']['enable'] = !!bootstrap
      PrivateChef['bootstrap']['enable'] = !!bootstrap
    end

    def gen_frontend
      PrivateChef[:role] = 'frontend'
      PrivateChef['bookshelf']['enable'] ||= false
      PrivateChef['bookshelf']['vip'] ||= PrivateChef['backend_vips']['ipaddress']
      PrivateChef['redis_lb']['enable'] ||= false
      PrivateChef['redis_lb']['vip'] ||= PrivateChef['backend_vips']['ipaddress']
      PrivateChef['elasticsearch']['enable'] ||= false
      PrivateChef['elasticsearch']['vip'] ||= PrivateChef['backend_vips']['ipaddress']
      PrivateChef['postgresql']['enable'] ||= false
      PrivateChef['postgresql']['vip'] ||= PrivateChef['backend_vips']['ipaddress']
      PrivateChef['lb']['upstream'] ||= Mash.new
      PrivateChef['lb']['upstream']['bookshelf'] ||= if PrivateChef['use_ipv6'] && PrivateChef['backend_vips']['ipaddress'].include?(':')
                                                       [ "[#{PrivateChef['backend_vips']['ipaddress']}]" ]
                                                     else
                                                       [ PrivateChef['backend_vips']['ipaddress'] ]
                                                     end
      PrivateChef['opscode_chef_mover']['enable'] = false
      PrivateChef['bootstrap']['enable'] = false
    end

    def gen_api_fqdn
      PrivateChef['lb']['api_fqdn'] ||= PrivateChef['api_fqdn']
      PrivateChef['lb']['web_ui_fqdn'] ||= PrivateChef['api_fqdn']
      PrivateChef['nginx']['server_name'] ||= PrivateChef['api_fqdn']
      PrivateChef['nginx']['url'] ||= "https://#{PrivateChef['api_fqdn']}"
    end

    # TODO: 2017-02-28 mp:  configurable location:
    def secrets_json
      '/etc/opscode/private-chef-secrets.json'
    end

    def credentials
      @credentials ||= Veil::CredentialCollection::ChefSecretsFile.new(path: secrets_json)
    end

    def gen_secrets(_node_name)
      # TODO
      # Transition from erchef's sql_user/password etc living under 'postgresql'
      # in older versions to 'opscode_erchef' in newer versions
      if credentials['postgresql'] && credentials['postgresql']['sql_password']
        credentials.delete('opscode_erchef', 'sql_password')
        credentials.add('opscode_erchef', 'sql_password', value: credentials['postgresql']['sql_password'].value)
        credentials.delete('postgresql', 'sql_password')
        credentials.delete('postgresql', 'sql_user')
      end

      required_secrets = [
        { group: 'postgresql', name: 'db_superuser_password', length: 100, set_command: 'set-db-superuser-password' },
        { group: 'redis_lb', name: 'password', length: 100 },
        { group: 'opscode_erchef', name: 'sql_password', length: 60 },
        { group: 'opscode_erchef', name: 'sql_ro_password', length: 60 },
        { group: 'opscode_erchef', name: 'stats_password', lendth: 100 },
        { group: 'oc_bifrost', name: 'superuser_id', length: 32, frozen: true },
        { group: 'oc_bifrost', name: 'sql_password', length: 100 },
        { group: 'oc_bifrost', name: 'sql_ro_password', length: 100 },
        { group: 'oc_id', name: 'secret_key_base', length: 100 },
        { group: 'oc_id', name: 'sql_password', length: 100 },
        { group: 'oc_id', name: 'sql_ro_password', length: 100 },
        { group: 'bookshelf', name: 'access_key_id', length: 40 },
        { group: 'bookshelf', name: 'secret_access_key', length: 80 },
        { group: 'bookshelf', name: 'sql_password', length: 80 },
        { group: 'bookshelf', name: 'sql_ro_password', length: 80 },
      ]

      optional_secrets = [
        { group: 'ldap', name: 'bind_password' },
        { group: 'data_collector', name: 'token' },
      ]

      optional_secrets.each do |secret|
        add_secret(secret, false)
      end

      required_secrets.each do |secret|
        add_secret(secret)
      end

      save_credentials_to_config if PrivateChef['insecure_addon_compat']
      credentials.save
    end

    def add_secret(secret_spec, create_if_missing = true)
      group = secret_spec[:group]
      name = secret_spec[:name]

      config_value = PrivateChef[group][name]

      if config_value
        warn_if_cred_mismatch(secret_spec)
        credentials.add(group, name, value: config_value, frozen: true, force: true)
      elsif create_if_missing
        credentials.add(group, name, length: secret_spec[:length], frozen: secret_spec[:frozen])
      end
    end

    def warn_if_cred_mismatch(opts)
      group = opts[:group]
      name = opts[:name]
      return unless credentials.exist?(group, name)

      pass_in_config = opts[:config_value] || PrivateChef[group][name]
      pass_in_secrets = opts[:secrets_value] || credentials.get(group, name)
      command_name = opts[:set_command] || "set-secret #{group} #{name}"
      config_key_desc = opts[:config_key_desc] || "#{group}['#{name}']"

      if pass_in_secrets != pass_in_config
        warning = <<~WARN
          #{config_key_desc} in secrets store does not match the value
          configured in chef-server.rb -- overriding secrets store password with
          configuration file password.
        WARN
        if command_name
          warning << <<~WARN2
            If this is unexpected, consider removing the secret from
            chef-server.rb and setting the correct value with:

                chef-server-ctl #{command_name}
          WARN2
        end
      elsif pass_in_config
        unless PrivateChef['insecure_addon_compat']
          warning = <<~WARN
            #{config_key_desc} has been saved to the secrets store
            but is also still in chef-server.rb. Please remove this from
            chef-server.rb.
          WARN
        end
      end

      ChefServer::Warnings.warn warning if warning
    end

    #
    # This has the end-result of making the credentials accessible via
    # node attributes in the cookbooks, since the PrivateChef
    # configuration object is eventually merged into the node object.
    #
    def save_credentials_to_config
      credentials.legacy_credentials_hash.each do |service, creds|
        # Ignore secrets added by add-ons and the keys
        next if PrivateChef[service].nil?

        creds.each do |name, value|
          PrivateChef[service][name] ||= value
        end
      end
    end

    def gen_redundant(node_name, _topology)
      me = PrivateChef['servers'][node_name]
      case me['role']
      when 'backend'
        gen_backend(me['bootstrap'])
        gen_api_fqdn
      when 'frontend'
        gen_frontend
        gen_api_fqdn
      else
        raise "I dont have a role for you! Use 'backend' or 'frontend'."
      end
    end

    def ensure_bind_password
      # if bind_dn is not set, don't care if there's a bind_password
      return unless PrivateChef['ldap'].key?('bind_dn')

      has_in_secrets = credentials.exist?('ldap', 'bind_password')

      # gen_secrets takes care of moving the secret from config to bind_password
      # (if it's not already in the secrets store, having been put there
      # manually)
      unless has_in_secrets
        raise 'Missing required LDAP config value bind_password (required when configuring bind_dn)'
      end
    end

    def gen_ldap
      required_ldap_config_values = %w( host base_dn )
      ensure_bind_password

      PrivateChef['ldap']['system_adjective'] ||= 'AD/LDAP'
      required_ldap_config_values.each do |val|
        unless PrivateChef['ldap'].key?(val)
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
        when 'simple_tls'
          ssl_enabled = true
        when 'start_tls'
          tls_enabled = true
        when 'none'
          Chef::Log.info('Configuring ldap without encryption.')
        else
          raise "Invalid ldap configuration: unknown value #{ldap_encryption} for deprecated ldap['encryption'] option. "\
                "Please set ldap['ssl_enabled'] = true or ldap['tls_enabled'] = true instead"
        end
      else
        if ssl_enabled && tls_enabled
          raise "Invalid ldap configuration: ldap['ssl_enabled'] and ldap['tls_enabled'] are mutually exclusive."
        end
      end
      PrivateChef['ldap']['ssl_enabled'] = ssl_enabled
      PrivateChef['ldap']['tls_enabled'] = tls_enabled
      PrivateChef['ldap']['encryption_type'] = if ssl_enabled
                                                 'simple_tls'
                                               elsif tls_enabled
                                                 'start_tls'
                                               else
                                                 'none'
                                               end
    end

    # True if the given topology requires per-server config via `server` blocks
    def server_config_required?
      PrivateChef['topology'] == 'tier'
    end

    def assert_server_config(node_name)
      unless PrivateChef['servers'].key?(node_name)
        Chef::Log.fatal <<~EOF
          No server configuration found for "#{node_name}" in /etc/opscode/chef-server.rb.
          Server configuration exists for the following hostnames:

            #{PrivateChef['servers'].keys.sort.join("\n  ")}

        EOF
        exit!(1)
      end
    end

    def generate_config_for_topology(topology, node_name)
      case topology
      when 'ha'
        Chef::Log.fatal <<~EOF
          DRBD_HA_002: Topology "ha" no longer supported.
          The DRBD/keepalived based HA subsystem was deprecated as of Chef Server
          12.9, and officially reached end of life on 2019-03-31. It has been
          disabled in Chef Server 13.

          See this post for more details:
          https://blog.chef.io/2018/10/02/end-of-life-announcement-for-drbd-based-ha-support-in-chef-server/

          What are my options?

          Chef Backend was announced over two years ago and is the recommended solution
          for all customers. It is a licensed product and available under the terms
          of a Chef Infra License.

          For more information on migrating from DRBD HA to Chef Backend or other HA, see this blog
          post and webinar: Best Practices for Migrating your Chef Server at
          https://blog.chef.io/2018/04/06/best-practices-for-migrating-your-chef-server/

          Customers in cloud environments are also encouraged to look at AWS OpsWorks
          and the Chef Automate Managed Service for Azure.

        EOF
        exit!(1)
      when 'standalone', 'manual'
        PrivateChef[:api_fqdn] ||= node_name
        gen_api_fqdn
      when 'tier'
        gen_redundant(node_name, topology)
      else
        Chef::Log.fatal("I do not understand topology #{PrivateChef.topology} - try standalone, manual, or tier.")
        exit 55
      end
    end

    def add_key_from_file_if_present(group, name, path)
      if File.readable?(path)
        credentials.add_from_file(path, group, name)
        true
      else
        false
      end
    end

    def migrate_solr4_settings
      # Many users were previously using external Elasticsearch by setting
      # the `opscode_solr4` configuration keys.  We don't want those
      # customers to have to update their configuration, so we copy over
      # opscode_solr4 external configuration if it exists.
      keys_to_migrate = {
        "external" => "external",
        "external_url" => "external_url",
        "elasticsearch_shard_count" => "shard_count",
        "elasticsearch_replica_count" => "replica_count"
      }
      keys_to_migrate.each do |old, new|
        if opscode_solr4.key?(old) && !elasticsearch.key?(new)
          elasticsearch[new] = opscode_solr4[old]
        end
      end
    end

    # If known private keys are on disk, add them to Veil and commit them.
    def migrate_keys
      did_something = add_key_from_file_if_present('chef-server', 'superuser_key', '/etc/opscode/pivotal.pem')
      did_something |= add_key_from_file_if_present('chef-server', 'webui_key', '/etc/opscode/webui_priv.pem')
      # Ensure these are committed to disk before continuing -
      # the secrets recipe will delete the old files.
      credentials.save if did_something
    end

    # When insecure_addon_compat is true, PrivateChef["ldap"] may be
    # non-empty after the secrets from veil are merged into the
    # configuration. The user may have inadvertantly left the password
    # in the secrets file, even when all other LDAP configuration has
    # been removed. This function is called before the secrets merge
    # allowing us to detect whether the user wants ldap or not.
    def set_ldap_enabled
      PrivateChef['ldap']['enabled'] = !PrivateChef['ldap'].empty?
    end

    def generate_config(node_name)
      assert_server_config(node_name) if server_config_required?
      set_ldap_enabled
      gen_secrets(node_name)
      migrate_keys

      # Under ipv4 default to 0.0.0.0 in order to ensure that
      # any service that needs to listen externally on back-end
      # does so.
      PrivateChef['default_listen_address'] = '0.0.0.0'
      # 'ipv4, ipv6, maybe add both
      case PrivateChef['ip_version']
      when 'ipv4', nil
        PrivateChef['use_ipv4'] = true
        PrivateChef['use_ipv6'] = false
        PrivateChef['default_listen_address'] = '0.0.0.0'
      when 'ipv6'
        PrivateChef['use_ipv4'] = false
        PrivateChef['use_ipv6'] = true
        PrivateChef['default_listen_address'] = '::'
      end

      # Transition Solr memory and JVM settings from OSC11 to Chef 12.
      import_legacy_service_config('opscode_solr', 'opscode_solr4', %w(heap_size new_size java_opts))
      deprecated_postgresql_settings
      migrate_solr4_settings
      transform_to_consistent_types

      PrivateChef['nginx']['enable_ipv6'] ||= PrivateChef['use_ipv6']

      generate_config_for_topology(PrivateChef['topology'], node_name)

      gen_ldap if PrivateChef['ldap']['enabled']
      generate_hash
    end

    def set_target_array_if_not_nil(target_array, key, value)
      unless value.nil?
        target_array[key] = value
      end
    end
  end
end
