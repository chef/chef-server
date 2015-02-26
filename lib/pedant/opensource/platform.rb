# Copyright: Copyright (c) 2012 Opscode, Inc.
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

require 'pedant/platform'

module Pedant
  class OpenSourcePlatform < Platform

    attr_reader :admin_requestors, :normal_requestors, :webui, :webui_key_file

    def initialize(server, superuser_key_file, superuser_name=platform.admin_client_name)
      @webui_key_file = Pedant::Config.webui_key || (fail "Missing webui_key in Pedant config.")
      @webui = Pedant::Requestor.new(superuser_name, superuser_key_file, platform: self)
      super(server, superuser_key_file, superuser_name)
    end

    def api_url(path_fragment = '')
      slash = path_fragment.start_with?('/') ? '' : '/'
      "#{@server}#{slash}#{path_fragment}"
    end

    def setup(requestors=Pedant::Config.requestors)
      requestors[:clients].each do |kind, client_hash|
        client_hash[:admin] = !!client_hash[:admin] # Convert to true or false

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
        key = cache_key(kind, :user)
        user_from_config(user_hash).tap do |user|
          requestor_cache[key] = user
        end
        create_requestor_accessor(key)
      end
    rescue Exception => e
      puts "Exception during Pedant credentials setup"
      puts e.inspect
      puts e.backtrace
      exit
    end

    def cleanup
      cleanup_requestors
    end

    # Open source clients can have an admin flag set
    def create_client(name, options = {})
      fail "Cannot create a client that is both an admin and a validator." if options[:admin] && options[:validator]

      # Explicitly declare this to pass on to the API
      options[:admin]     ||= false
      options[:validator] ||= false

      clientname = name.to_s
      puts "Creating client #{clientname}..."
      payload = {
        "name"      => clientname,
        "admin"     => options[:admin],
        "validator" => options[:validator]
      }

      r = post(api_url('/clients'), @superuser, :payload => payload)

      case r.code
      when 201 # Created
        # OK
      when 409 # Conflict
        puts "The client #{clientname} already exists... regenerating a key for it now"
        payload["private_key"] = true
        r = put(api_url("/clients/#{clientname}"), @superuser, :payload => payload)
      else
        # Something bad happened that Pedant can't recover from
        message = <<-EOM
Encountered an error attempting to create client #{clientname}
Response Code was: #{r.code}
Response Body was: #{r}
EOM
        # Because RSpec swallows exception messages raised in before
        # blocks, and then goes on to run all after blocks, the run is
        # going to fail, but with a cryptic message along the lines of
        # "delete_client': undefined method `preexisting' for
        # nil:NilClass (NoMethodError)".  This will at least allow
        # some information to be printed out to help elucidate the
        # underlying issue
        puts message
        raise
      end

      private_key = parse(r)["private_key"]
      Pedant::Client.new(clientname, private_key, platform: self, preexisting: false, admin: options[:admin], validator: options[:validator])
    end

    def client_from_config(requestor_spec)
      name = requestor_spec[:name]
      create_me = requestor_spec[:create_me]
      type = requestor_spec[:type]
      admin = requestor_spec[:admin]
      validator = requestor_spec[:validator]
      key_file = requestor_spec[:key_file]

      # Extract to after hooks
      if create_me
        create_client(name, admin: admin, validator: validator).tap do |client|
          client.populate_dot_chef! if requestor_spec[:create_knife]
        end
      else
        Pedant::Client.new(name, key_file, platform: self, preexisting: true, admin: admin, validator: validator)
      end
    end

    def delete_client(client)
      if client.preexisting
        puts "Pedant did not create the client #{client.name}, and will not delete it"
      else
        puts "Deleting client #{client.name} ..."
        r = delete(api_url("/clients/#{client.name}"), @superuser)
        if r.code != 200
          puts "Unexpected response #{r.code}: #{r}"
        end
      end
    end

    def create_user(username, options = {})
      # email can be sent for user and will be set in database, but won't be returned back
      # openid can also be sent and will be returned back
      payload = {
        "name" => username,
        #"email" => "#{username}@opscode.com",
        #"openid" => "",
        "password" => "foobar",
        "admin" => options[:admin]
      }
      users_url = "#{@server}/users"

      r = post(users_url, @superuser, :payload => payload)
      if r.code == 409
        puts "The user #{username} already exists... regenerating a key for it now"
        payload["private_key"] = true
        r = put("#{users_url}/#{username}", @superuser, :payload => payload)
      end

      private_key = parse(r)["private_key"]

      Pedant::User.new(username, private_key, platform: self, preexisting: false)
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

    def user_from_config(requestor_spec)
      name = requestor_spec[:name]
      create_me = requestor_spec[:create_me]
      key_file = requestor_spec[:key_file]
      is_admin = !!requestor_spec[:admin]

      if create_me
        create_user(name, admin: is_admin).tap do |user|
          user.populate_dot_chef! if requestor_spec[:create_knife]
        end
      else
        Pedant::User.new(name, key_file, platform: self, preexisting: true)
      end
    end

  end # class
end # Module
