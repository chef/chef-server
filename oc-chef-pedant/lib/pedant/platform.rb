# Copyright: Copyright 2012-2018 Chef Software, Inc.
# License: Apache License, Version 2.0
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

require 'uri'
require 'pathname'
require 'fileutils'
require 'pedant/acl'
module Pedant

  # Representation of the Chef Server platform
  class Platform
    include Pedant::Request

    GLOBAL_OBJECTS = ['users', 'organizations']
    MAX_ATTEMPTS = 5

    attr_reader :test_org, :test_org_owner, :validate_org, :internal_account_url,
                :internal_server, :ldap, :ldap_testing,
                :server, :base_resource_url, :superuser, :superuser_key_data, :webui_key,
                :stats_user, :stats_password

    # Create a Platform object for a given server (specified by
    # protocol, hostname, and port ONLY). You must supply the
    # superuser's key data in PEM form.
    #
    def initialize(server, superuser_key, webui_key, superuser_name='pivotal', stats_user, stats_password)
      @superuser_key_data = superuser_key
      @webui_key = webui_key
      @server = (Pedant.config.explicit_port_url ? explicit_port_url(server) : server )
      puts "Configured server URL: #{@server}"

      @base_resource_url = Pedant::Config.base_resource_url || @server
      puts "Configured base resource URL: #{@base_resource_url}"

      @superuser = Pedant::Requestor.new(superuser_name, @superuser_key_data, platform: self)
      @test_org = org_from_config

      @stats_user = stats_user
      @stats_password = stats_password

      @internal_account_url = Pedant::Config[:internal_account_url]
      @internal_server = Pedant::Config.internal_server || (fail "Missing internal_server in Pedant config.")
      @ldap = Pedant::Config[:ldap]
      @ldap_testing = Pedant::Config[:ldap_testing]
      self.pedant_run_timestamp # Cache the global timestamp at initialization
      self
    end

    DEFAULT_ORG_REWRITE = /^\/?(search|nodes|cookbooks|data|roles|sandboxes|environments|clients|principals|runs|groups|containers|keys)/
    def map_to_default_orgname?(path_fragment)
      return false unless Pedant::Config.use_default_org # Don't even bother unless we are in default_orgname mode
      return false if path_fragment =~ /_acl/            # False if _acl appears anywhere
      return true  if path_fragment =~ DEFAULT_ORG_REWRITE
      return false                                       # Default to false
    end

    # Intelligently construct a complete API URL based on the
    # pre-configured server and platform information.  URLs targeted for
    # multi-tenant platforms (i.e. Hosted Chef) prepend
    # "/organizations/#{org}" to the given path fragment, while
    # single-tenant targeted URLs (i.e., Open Source Chef and Private
    # Chef) do not.
    def api_url(path_fragment = '/', org=test_org)
      path_prefix = (map_to_default_orgname?(path_fragment) ? '' : "/organizations/#{org.name}")
      slash = path_fragment.start_with?('/') ? '' : '/'
      "#{server}#{path_prefix}#{slash}#{path_fragment}"
    end

    # Construct a complete API URL for internal APIs that are not typically exposed
    # via the public-facing load balancers.   Other than the server itself,
    # behaves the same as api_url
    def internal_api_url(path_fragment = '/', org=test_org)
      path_prefix = "/organizations/#{org.name}"
      slash = path_fragment.start_with?('/') ? '' : '/'
      "#{internal_server}#{path_prefix}#{slash}#{path_fragment}"
    end

    # Construct the appropriate URL for returned resources
    #
    def resource_url(path_fragment = '/', org=test_org)
      "#{@base_resource_url}#{path_helper(path_fragment, org)}"
    end

    # Helper for above
    def path_helper(path_fragment, org)
      orgname = org.respond_to?(:name) ? org.name : org.to_s
      path_prefix = "/organizations/#{orgname}"
      slash = path_fragment.start_with?('/') ? '' : '/'
      "#{path_prefix}#{slash}#{path_fragment}"
    end

    ################################################################################
    # Config-Aware Operations
    #
    # These operations for adding and deleting users and orgs respect
    # config settings; i.e. they will not create new users or orgs if
    # the config indicates they should already exist, and they won't
    # delete them unless Pedant created them in the first place.
    #
    ################################################################################

    def pedant_orgname
      Pedant::Config.use_default_org ? Pedant::Config.default_orgname : Pedant::Config[:org][:name]
    end

    def user_from_config(requestor_spec)
      name = requestor_spec[:name]
      create_me = requestor_spec[:create_me]
      key_file = requestor_spec[:key_file]
      associate = requestor_spec[:associate]
      admin  = requestor_spec[:admin]

      if create_me
        create_user(name, admin: admin, associate: associate).tap do |user|
          user.populate_dot_chef! if requestor_spec[:create_knife]
        end
      else
        # the user is expected to exist, so we won't actually create it in chef
        # server, but still allowing us to use it as a requestor.
        Pedant::User.new(name, key_file, platform: self, preexisting: true)
      end
    end

    def org_from_config()
      org = Pedant::Config[:org]
      # If default_orgname is set, override the settings for org
      name = pedant_orgname
      if Pedant::Config.use_default_org
        # org validation also associates a pedant org owner
        # with the org as an admin. This is eq
        @validate_org = true
        create_org(name)
      elsif org[:create_me]
        @validate_org = !!Pedant::Config.validate_org_creation
        create_org(name)
      else
        key = org[:validator_key]
        Pedant::Organization.new(name, key)
      end
    end

    def delete_org_from_config
      if Pedant.config[:org][:create_me] && Pedant.config[:delete_org]
        delete_org(pedant_orgname)
      else
        puts "Pedant did not create the org, so will not delete it"
      end
    end
    # TODO can these just live in 'rspec/common'?
    def org_name
      test_org.name
    end
    def validator_client
      test_org.validator
    end
    def validator_client_name
      "#{org_name}-validator"
    end

    # Since Erchef will now return URLs based upon the Host: header, and it receives
    # a fully-qualified url with an explicit port, we need to normalize the server url
    # so that port numbers are added, even if the url are default port
    def explicit_port_url(url)
      uri = URI.parse(url)
      return url unless uri.default_port == uri.port
      return "#{url}:#{uri.port}"
    end

    def setup(requestors=Pedant::Config.requestors)
      requestors[:clients].each do |kind, client_hash|

        key = cache_key(kind, :client)

        requestor_cache[key] = unless client_hash[:bogus]
                                 client_from_config(client_hash)
                               else
                                 dummy_client(client_hash)
                               end
        create_requestor_accessor(key)

      end

      requestors[:users].each do |kind, user_hash|
        user_hash[:admin] = !!user_hash[:admin] # Convert to true or false
        user_hash[:associate] = true if user_hash[:associate].nil? # default to associate
        user_from_config(user_hash).tap do |user|

          key = cache_key(kind, :user)

          requestor_cache[key] = user
          make_owner(user, @test_org) if user_hash[:admin]
          make_user(user, @test_org) if user_hash[:associate] and !user_hash[:admin]

          create_requestor_accessor(key)
        end
      end

    end

    def cleanup
      cleanup_requestors
      delete_org_from_config
    end

    def create_client(name, org = self.test_org)
      clientname = name.to_s
      payload = { "name" => clientname }

      if server_api_version == 0
        r = post(api_url('/clients', org), org.validator, :payload => payload)
        if r.code == 409
          payload["private_key"] = true
          r = put(api_url("/clients/#{clientname}", org), org.validator, :payload => payload)
        end
        private_key = parse(r)["private_key"]
      else
        r = post(api_url('/clients', org), org.validator, :payload => payload.with("create_key", true))
        if r.code == 409
          r = put(api_url("/clients/#{clientname}/keys/default", org), org.validator, :payload => { "create_key" => true} )
        end
        private_key = parse(r)["chef_key"]["private_key"]
      end
      Pedant::Client.new(clientname, private_key, platform: self)
    end

    def client_from_config(requestor_spec)
      name = requestor_spec[:name]
      create_me = requestor_spec[:create_me]
      key_file = requestor_spec[:key_file]

      # Extract to after hooks
      if create_me
        create_client(name).tap do |client|
          client.populate_dot_chef! if requestor_spec[:create_knife]
        end
      else
        Pedant::Client.new(name, key_file, platform: self, preexisting: true, admin: false)
      end
    end

    def delete_client(client, org = self.test_org)
      r = delete("#{@server}/organizations/#{org.name}/clients/#{client.name}", self.superuser)
      if r.code != 200
        puts "Unexpected response #{r.code}: #{r}"
      end
    end

    def test_repository_path
      @_test_respository_path ||= Pedant::Utility.fixture_path('test_repository')
    end

    def requestor_cache
      @_requestor_cache ||= {}
    end

    # Generate a symbol key for inserting / retrieving a requestor
    # from the requestor cache.  +config_key+ should be the key under
    # which a requestor hash is stored in the Pedant config, while
    # +type+ should be either :user or :client.
    #
    # For example, if you have, say, a config entry like
    #
    #    Pedant.config['requestors']['clients']['admin']
    #
    # you would pass in 'admin' for +config_key+.
    def cache_key(config_key, type)
      valid_types = [:user, :client]
      unless valid_types.include?(type)
        raise "Can only create a cache key for #{valid_types}; you gave #{type.inspect}"
      end
      "#{config_key}_#{type}".to_sym
    end

    # Generate an accessor method to easily get a requestor that has
    # been generated from a Pedant config file.  The +cache_key+
    # should be generated from the +cache_key+ method.
    #
    # For example, if you have, say, a config entry like
    #
    #    Pedant.config['requestors']['clients']['admin']
    #
    # then you will be able to access that user via
    #
    #    platform.admin_client
    #
    # Similar logic holds for users as well.
    def create_requestor_accessor(cache_key)
      self.class.send(:define_method, cache_key, lambda{requestor_cache[cache_key]})
    end

    def cleanup_requestors
      # Don't need to delete bogus clients, because it wasn't
      # associated with the server to begin with
      requestor_cache.values.reject(&:bogus?).each(&:delete!)
    end

    def users
      requestor_cache.values.select { |r| Pedant::User === r }
    end

    def clients
      requestor_cache.values.select { |r| Pedant::Client === r }
    end


    def dummy_client(h)
      Pedant::Client.new(h[:name], bogus_key, platform: self, preexisting: false, bogus: true)
    end

    # Normalize timestamps used in Pedant
    # In tests, you can access this with:
    # let(:timestamp) { platform.now }
    def timestamp
      Time.now.utc
    end

    alias now timestamp

    # Global timestamp marking the beginning of the run
    # Use this for things like generating unique names
    def pedant_run_timestamp
      @_pedant_run_timestamp ||= self.timestamp
    end

    # TODO: Expose entire payload as an input parameter
    def create_org(orgname)
      payload = {
        "name" => orgname,
        "full_name" => orgname,
        "org_type" => "Business"
      }
      puts "Creating org #{orgname}"
      MAX_ATTEMPTS.times do |attempt|
        r = post("#{@server}/organizations", superuser, :payload => payload)

        case r.code
        when 201, 200
          parsed = parse(r)
          validator_name = parsed["clientname"]
          validator_key = parsed["private_key"]

          validator = Pedant::Client.new(validator_name, validator_key)
          return Pedant::Organization.new(orgname, validator)
        when 503
          # Continue attempting by allowing the loop to continue
          puts "Failed attempting to contact #{@server} (#{attempt}/#{MAX_ATTEMPTS})"
        else
          raise "Bad error code #{r.code} from create org: #{r}"
        end

      end
      raise "Failed attempting to contact #{@server} #{MAX_ATTEMPTS} times"
    end

    def get_org(orgname, validator_name = nil, validator_key = nil)
      validator = nil
      validator = Pedant::Client.new(validator_name, validator_key) unless validator_name.nil?
      Pedant::Organization.new(orgname, validator)
    end

    def delete_org(orgname)
      r = delete("#{@server}/organizations/#{orgname}/clients/#{orgname}-validator", superuser)
      if r.code != 200 and r.code != 404
        puts "Unexpected response when deleting org validator: #{r.code}: #{r}"
      end
      puts "Deleting organization #{orgname} ..."
      r = delete("#{@server}/organizations/#{orgname}", superuser)
      if r.code != 200
        puts "Unexpected response when deleting org: #{r.code}: #{r}"
      end
    end

    def create_min_user(username, options = {})
      payload = {
        "username" => username,
        "email" => "#{username}@chef.io",
        "first_name" => username,
        "last_name" => username,
        "display_name" => username,
        "password" => "foobar"
      }
      if options.has_key?(:overrides)
        payload = payload.merge(options[:overrides])
      end
      users_url = "#{@server}/users"
      post(users_url, @superuser, :payload => payload)

    end
    def create_user(username, options = {})
      r = create_min_user(username, options)
      raise "Could not create user #{username}: #{r}" unless r.code == 201
      if server_api_version == 0
        private_key = parse(r)["private_key"]
      else
        private_key = parse(r)["chef_key"]["private_key"]
      end

      # The "admin" and "associate" options here are more of a metadata
      # than actually creating an admin or associating. This allows
      # Pedant tests to succeed even if the users config table has changed.

      Pedant::User.new(username, private_key, platform: self, preexisting: false, admin: options[:admin], associate: options[:associate])
    end

    def delete_user(user)
      if user.preexisting
        puts "Pedant did not create the user #{user.name}, and will not delete it"
      else
        r = delete("#{@server}/users/#{user.name}", @superuser)
        if r.code != 200
          puts "Unexpected response deleting user #{user.name} - #{r.code}: #{r}"
        end
        r
      end
    end

    def remove_user_from_org(orgname, user)
      delete("#{@server}/organizations/#{orgname}/users/#{user.name}", superuser)
    end

    def associate_user_with_org(orgname, user)
      r = post("#{@server}/organizations/#{orgname}/users", superuser, :payload => { "username" => user.name })
      if r.code == 201 or (r.code == 409 and parse(r)["error"] == "The association already exists.")
        r
      else
        raise "Bad response #{r.code} from POST /organizations/#{orgname}/users w/ { 'username' : '#{user.name}'}: #{r}"
      end
    end

    # the following set of methods could benefit from some
    # metaprogramming magic, but I think a refactor of the
    # user calls to accept just a username instead of a user
    # object would be in order.
    def add_user_to_group(orgname, user, groupname, actor=nil)
      alter_group(orgname, groupname, :add, :user, user.name, actor)
    end

    def remove_user_from_group(orgname, user, groupname, actor=nil)
      alter_group(orgname, groupname, :remove, :user, user.name, actor)
    end

    def add_client_to_group(orgname, client, groupname, actor=nil)
      alter_group(orgname, groupname, :add, :client, client.name, actor)
    end

    def remove_client_from_group(orgname, client, groupname, actor=nil)
      alter_group(orgname, groupname, :remove, :client, client.name, actor)
    end

    def add_group_to_group(orgname, object_name, groupname, actor=nil)
      alter_group(orgname, groupname, :add, :group, object_name, actor)
    end

    def remove_group_from_group(orgname, object_name, groupname, actor=nil)
      alter_group(orgname, groupname, :remove, :group, object_name, actor)
    end

    def alter_group(orgname, groupname, action, object_type, object_name, actor=nil)
      # since we can't set the default param to an attr_accessor, this will have
      # to suffice
      actor ||= superuser

      type_map = { :user => :users, :group => :groups, :client => :clients }

      group_url = "#{@server}/organizations/#{orgname}/groups/#{groupname}"
      r = get(group_url, actor)
      group = parse(r)

      payload = {
        :groupname => groupname,
        :actors => {
          :users => group["actors"],
          :groups => group["groups"],
          :clients => group["clients"]
        }
      }

      case action
      when :add
        payload[:actors][type_map[object_type]].unshift(object_name)
      when :remove
        payload[:actors][type_map[object_type]].delete(object_name)
      end

      put(group_url, actor, :payload => payload)
    end

    # As though +user+ had created +org+ themselves
    def make_owner(user, org)
      associate_in_groups(user, org, ["admins", "billing-admins", "users"])
    end

    def make_user(user, org)
      associate_in_groups(user, org, ["users"])
    end

    # Helper function to associate a user with an org, and place the
    # user in the specified groups
    def associate_in_groups(user, org, groups)
      associate_user_with_org(org.name, user)
      groups.each do |group|
        add_user_to_group(org.name, user, group)
      end
    end


    def before_configure_rspec
      # Note that validate_created_org is also responsible for associating an
      # owner with the org.
      validate_created_org(test_org) if validate_org
    end

    def configure_rspec
      # Create a path for all of our generated content. Makes it easier
      # to do a final cleanup pass
      FileUtils.mkpath File.join(Dir.tmpdir, "oc-chef-pedant")
      ::RSpec.configure do |c|
        c.run_all_when_everything_filtered = true
        c.filter_run_excluding :intermittent_failure => true
        c.include Pedant::ACL
      end
    end


    def validate_created_org(org)
      puts "Validating Org Creation"

      @test_org_owner = create_user("#{org.name}_owner", associate: true, admin: true)
      requestor_cache[:owner] = @test_org_owner
      make_owner(self.test_org_owner, org)

      ::RSpec.configure do |c|
        # If you just want to run one (or a few) tests in development,
        # add :focus metadata
        c.filter_run :focus => true
        c.run_all_when_everything_filtered = true
        c.include Pedant::RSpec::Common
      end

      args = if Pedant.config.debug_org_creation
               Pedant.config.rspec_formatting_args
             else
               []
             end
      if Pedant.config[:tags]
        args.concat(Pedant.config[:tags].map { |tag| ['-t', tag.to_s] } )
      end
      args.concat(Pedant::Gem.test_directories("org_creation"))

      if ::RSpec::Core::Runner.run(args.flatten) > 0
        delete_org_from_config
        delete_user(@test_org_owner) if @test_org_owner
        puts "Error: unable to validate testing org"
        exit 2
      end

      # We need to reset RSpec after using it. Below are the hacks necessary for reset
      ::RSpec.reset
      ::RSpec.configuration.extend RSpecShared::Methods
    end
    # Well-formed key, but not actually associated with the server
    def bogus_key
      @_bogus_key ||=
"-----BEGIN RSA PRIVATE KEY-----
MIIEogIBAAKCAQEA7TfhToresZudM5gaBfzM/eHrGuJtN8uMaG51fLk9rNVwOxIw
eb9AmWJSQgftRj4AJSnt8Jv+QyAafjg79rEmCpd2K+toN1fXiVsf/ld/VdMI5vCd
usJc8aC4OFIVrLetm9eq+joXiCSLfEYP1w4d1gc9wqr54rnGVgQdWv31NhqXX7Tl
2bHRoEsfIFM7Vr5zC9Rv6XEtrwezSpzQ+ru707UOCOHNnH1TDxNDz+L71xJnGXD6
uh596hVm2GiXJ8lyCVJTs/RZ+HKtRuxYOJztdCQAXbd6DDjTBax/cCEzzoxK/LMc
1K/zWCKN8yM/XpiFqNuBB0yrqS5Y4zlROY0ssQIDAQABAoIBAQDmC1HYvD1YGePq
O+/IrK8S6jr4WGq4OBIS2EPhTzbrXBU5g9s0xe7ckIfa9xr4CnpTkATqWCzMZd6r
Vtd31bVhgh6cWu829F3WG2O8YJfg4AX7B46+pWxC+qyMGbZhR8L5pb1ualWVtnL6
cms8D7mJbH5NQUeRwrz/f4AEVNGuw165PhglXnRq3zxs97jdNH6av0na1VLAmUeR
vmYToLoIsK5yLeNzMfRRrQkq4ndUvad2ahqsz+p/xUjfYneBz4uujE/PcCgi423Q
FMH1NtPpzmuZuABU53wUMW4BXsSGv+34UMhRTazcBPRBYjKT6l6F7Tt9wPFxrtVn
KYI9dI0tAoGBAPzBwLsUuyEJtSd2zW63f7EhhG/2rSBm1g9Bcs1sHZtXIX3JN51X
hd+ckXQcLgQYn8syGW1+WcuaHGyp7v3G0yr6b38FWwEnmD85Mq5da1dyio2udBdK
Xm8MfY94yP8qSH3bUDKEl+cV9X5rtzbQAorMXb/Qkc4vErWfABVBKeLPAoGBAPBD
FjGxER9YU4hFV2vn5qElfxa799Vl0uSvao0lSkTpA/sHFxAkRQc/mo1PBClaH0ba
Hqr141o5pGUDgLqpO3kEY5vYBOaFXLFdFCcL+1YUR6t0bX+WCHuq21Cs6Gu4+qNA
D4dpsSPDXfatyXWM5PF2d4FwO2XnL25Yt+rg6dh/AoGAbEfk9UCQWjr6bImswH3E
KnIddonK6VKk6aw0LmTe2imdo3GMbc+M/prohUF9RSv3aOlxk0LJ3TuMadDzHa0L
0iGvmk8FCZ2Yz50FZUWIMtJTIRdXjJLDmfdT4x7vnMDUhXZrCPlcyhbSMPKcbtL2
A9hBYWdMz3PDJCOVuYVNGGkCgYEAhSxKUwTYfs1Qj8oPqOoDdfL4vLs3pfsoqEVr
BA1VW1jlMfE+IV5ZPKlOm2De56TijT09nnloqYwlyS/l3JENPAjoxWs5XCUzucPj
9bi4eYAIMcr5Hq0kypdrtQ4CTiNcGbzaXq6A11fk72RotFWCWSzXFNIGuncoXTuj
xfcg5zUCf3TQrwpcBB1Hf9hm6lUVpptvUZJAQtkWoaWXUPRY0CjcVdDc5ak4xqL2
FcAuJwV95b3qLyPESJ8zUBLOg3DcT1X9mrvRuKgC/Ie6k4R+oxU/nXi4vsPXlvg4
yap6MUYSjPOa7eCrhg2zFZiqO6VLEogPc1nsjb9Zl2UWLLYyCVz=
-----END RSA PRIVATE KEY-----"
    end

    def gen_rsa_key(name)
      priv = Tempfile.new("pedant-key-#{name}")
      pub = Tempfile.new("pedant-key-#{name}.pub")
      `openssl genrsa -out #{priv.path} 2048 1>/dev/null 2>&1`
      `openssl rsa -in #{priv.path} -pubout -out #{pub.path} 2>/dev/null`
      key = { :path => "#{pub.path}",
              :private => File.read(priv.path),
              :public => File.read(pub.path) }

      priv.close
      priv.unlink
      pub.close
      pub.unlink
      key
    end
  end

end
