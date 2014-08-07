require 'pedant/platform'

module Pedant
  class MultiTenantPlatform < Platform

    GLOBAL_OBJECTS = ['users', 'organizations']
    MAX_ATTEMPTS = 5

    attr_reader :test_org, :test_org_owner, :validate_org, :internal_account_url

    def initialize(server, superuser_key_file, super_user_name='pivotal')
      super(server, superuser_key_file, super_user_name)
      @test_org = org_from_config
      @internal_account_url = Pedant::Config[:internal_account_url]
    end

    # Intelligently construct a complete API URL based on the
    # pre-configured server and platform information.  URLs targeted for
    # multi-tenant platforms (i.e. Hosted Chef) prepend
    # "/organizations/#{org}" to the given path fragment, while
    # single-tenant targeted URLs (i.e., Open Source Chef and Private
    # Chef) do not.
    def api_url(path_fragment = '/', org=test_org)
      path_prefix = "/organizations/#{org.name}"
      slash = path_fragment.start_with?('/') ? '' : '/'
      "#{server}#{path_prefix}#{slash}#{path_fragment}"
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

    # TODO: expose the entire payload as an input parameter
    def create_user(username, options = {})
      payload = {
        "username" => username,
        "email" => "#{username}@opscode.com",
        "first_name" => username,
        "last_name" => username,
        "display_name" => username,
        "password" => "foobar"
      }

      users_url = "#{@server}/users"

      r = post(users_url, @superuser, :payload => payload)
      if r.code == 409
        puts "The user #{username} already exists... regenerating a key for it now"
        payload["private_key"] = true
        r = put("#{users_url}/#{username}", @superuser, :payload => payload)
      end

      private_key = parse(r)["private_key"]

      # The "admin" and "associate" options here are more of a metadata
      # than actually creating an admin or associating. This allows
      # Pedant tests to succeed even if the users config table has changed.
      Pedant::User.new(username, private_key, platform: self, preexisting: false, admin: options[:admin], associate: options[:associate])
    end

    def delete_user(user)
      if user.preexisting
        puts "Pedant did not create the user #{user.name}, and will not delete it"
      else
        puts "Deleting user #{user.name} ..."
        ## TODO: use api_url
        r = delete("#{@server}/users/#{user.name}", @superuser)
        if r.code != 200
          puts "Unexpected response #{r.code}: #{r}"
        end
      end
    end

    ################################################################################
    # Config-Aware Operations
    #
    # These operations for adding and deleting users respect
    # config settings; i.e. they will not create new users or orgs if
    # the config indicates they should already exist, and they won't
    # delete them unless Pedant created them in the first place.
    #
    ################################################################################

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
        Pedant::User.new(name, key_file, platform: self, preexisting: true)
      end
    end

    # def delete_user_from_config(user_key)
    #   user = Pedant::Config[:users][user_key]
    #   name = user[:name]
    #   if user[:create_me]
    #     delete_user(name)
    #   else
    #     puts "Pedant did not create user #{name}, and will not delete it"
    #   end
    # end


    def create_client(name)
      clientname = name.to_s
      puts "Creating client #{clientname}..."
      payload = { "name" => clientname }

      r = post(api_url('/clients'), @test_org.validator, :payload => payload)

      if r.code == 409
        puts "The client #{clientname} already exists... regenerating a key for it now"
        payload["private_key"] = true
        r = put(api_url("/clients/#{clientname}"), @test_org.validator, :payload => payload)
      end

      private_key = parse(r)["private_key"]
      Pedant::Client.new(clientname, private_key, platform: self)
    end

    def client_from_config(requestor_spec)
      name = requestor_spec[:name]
      create_me = requestor_spec[:create_me]
      type = requestor_spec[:type]
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
      puts "Deleting client #{client.name} ..."
      r = delete("#{@server}/organizations/#{org.name}/clients/#{client.name}", self.admin_user)
      if r.code != 200
        puts "Unexpected response #{r.code}: #{r}"
      end
    end



    ################################################################################
    # Multi-Tenant Platform Methods
    #
    # Organization and Authorization-related (e.g., groups) operations go here.
    #
    # TODO: Extract this into a separate class
    ################################################################################

    # TODO: Expose entire payload as an input parameter
    def create_org(orgname)
      payload = {
        "name" => orgname,
        "full_name" => orgname,
        "org_type" => "Business"
      }

      MAX_ATTEMPTS.times do |attempt|
        r = post("#{@server}/organizations", superuser, :payload => payload)

        # This re-assigns the variable 'r' and therefore can't be part of the case statement below
        if r.code == 409
          puts "The organization already exists!  Regenerating validator key ..."
          r = post("#{Pedant::Config.account_server}/organizations/#{orgname}/_validator_key", superuser, {})
          raise "Bad error code #{r.code} from regenerating validator key: #{r}" unless r.code == 200
        end

        case r.code
        when 201, 200
          parsed = parse(r)
          # If we came here through the 409 codepath there won't be a client name so we're hardcoding it.
          validator_name = parsed["clientname"] || "#{orgname}-validator"
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

    def delete_org(orgname)
      puts "Deleting organization #{orgname} ..."
      r = delete("#{@server}/organizations/#{orgname}", superuser)
      if r.code != 200
        puts "Unexpected response #{r.code}: #{r}"
      end
    end

    def associate_user_with_org(orgname, user)
      puts "Associating user #{user.name} with org #{orgname} ..."
      payload = { "user" => user.name }
      association_requests_url = "#{@server}/organizations/#{orgname}/association_requests"
      r = post("#{association_requests_url}",  superuser, :payload => payload)

      if r.code == 201 # Created
        association_id = parse(r)["uri"].split("/").last
        r = put("#{@server}/users/#{user.name}/association_requests/#{association_id}", user, :payload => { "response" => "accept" })
      elsif r.code == 409 && parse(r)["error"] == "The association already exists."
        # No problem!
      else
        raise "Bad response #{r.code} from association_requests: #{r}"
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

    def add_group_to_group(orgname, object_name, groupname, actor=nil)
      alter_group(orgname, groupname, :add, :group, object_name, actor)
    end

    def remove_group_from_group(orgname, object_name, groupname, actor=nil)
      alter_group(orgname, groupname, :remove, :group, object_name, actor)
    end

    def alter_group(orgname, groupname, action, object_type, object_name, actor=nil)
      actor ||= superuser

      type_map = { :user => :users, :group => :groups }

      group_url = "#{@server}/organizations/#{orgname}/groups/#{groupname}"
      r = get(group_url, actor)
      group = parse(r)

      payload = {
        :groupname => groupname,
        :actors => {
          :users => group["actors"],
          :groups => group["groups"]
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

    ################################################################################
    # Config-Aware Operations
    #
    # These operations for adding and deleting users and orgs respect
    # config settings; i.e. they will not create new users or orgs if
    # the config indicates they should already exist, and they won't
    # delete them unless Pedant created them in the first place.
    #
    ################################################################################

    def org_from_config()
      org = Pedant::Config[:org]
      name = org[:name]
      if org[:create_me]
        @validate_org = true
        create_org(name)
      else
        key = org[:validator_key]
        puts "Using pre-created org. Skipping org creation validation tests."
        Pedant::Organization.new(name, key)
      end
    end

    def delete_org_from_config
      if Pedant.config[:org][:create_me] && Pedant.config[:delete_org]
        delete_org(Pedant.config[:org][:name])
      else
        puts "Pedant did not create the org, so will it not delete it"
      end
    end

    # When this is defined, pedant will run this before running anything else.
    def before_configure_rspec
      validate_created_org(test_org) if validate_org
    end

    def validate_created_org(org)
      puts "Validating Org Creation"

      @test_org_owner = create_user("#{org.name}_owner", associate: true, admin: true)
      requestor_cache[:owner] = @test_org_owner
      make_owner(self.test_org_owner, org)

      ::RSpec.configure do |c|
        c.treat_symbols_as_metadata_keys_with_true_values = true
        c.include Pedant::RSpec::Common
      end

      args = if Pedant.config.debug_org_creation
               Pedant.config.rspec_formatting_args
             else
               []
             end
      args.concat(Pedant::Gem.test_directories("org_creation"))

      if ::RSpec::Core::Runner.run(args) > 0
        delete_org_from_config
        delete_user(test_org_owner)
        puts "Error: unable to validate testing org"
        exit 2
      end

      # We need to reset RSpec after using it. Below are the hacks necessary for reset
      ::RSpec.reset
      ::RSpec.configuration.extend RSpecShared::Methods
    end

  end
end
