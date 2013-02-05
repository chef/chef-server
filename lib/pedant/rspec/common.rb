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

require 'json'
require 'rest_client'
require 'pedant/concern'

module Pedant
  module RSpec
    module Common
      extend Pedant::Concern
      # We use a concern instead of a shared context in order to access
      # the complete Rspec DSL, plus extensions (metadata, shared(), etc.)

      included do

        shared(:superuser){"00000000000000000000000000000000"}

        # JSON Utilities
        def parse(json_string)
          ::JSON.parse(json_string,
                       :create_additions => false,
                       # some keys don't play nicely as Ruby symbols (embedded hyphens, e.g.)
                       :symbolize_names => false)
        end

        def to_json(data)
          ::JSON.generate(data)
        end

        ####################################

        # TODO: This needs to be configurable
        def api_url(path_fragment)
          "http://127.0.0.1:6969#{path_fragment}"
        end

        # Helper method for creating a new Authz object.  Do not use
        # directly in tests.
        def new_item(type, requestor)
          r = post("/#{type}s", requestor, :payload => nil)
          r.code.should eq(201)
          parse(r)["id"]
        end

        # Helper method for deleting an Authz object.  Do not use
        # directly in tests.
        def delete_item(type, id_or_name)
          delete("/#{type}s/#{resolve(id_or_name)}", :superuser)
        end

        # Generate all creation helper methods
        #
        # NOTE: Doesn't include :container, because creating one
        # requires a POST body
        #
        # TODO: Create corresponding container methods
        [:actor, :object, :group].each do |type|

          # +with_TYPE+ accepts a Keyword / String argument and
          # creates a new instance of that type, storing the Authz ID
          # of the new object under a +let+ variable of the same name.
          #
          # So, if you want to create a new actor with the label
          # +:alice+, you would invoke
          #
          #  with_actor :alice
          #
          # In subsequent test examples, use the symbol +:alice+ in
          # all custom matchers.  The variable +alice+ will also be
          # available, but should only really be used to create URL
          # fragments via string interpolation (don't worry; the
          # custom matchers will tell you what to do if you do the
          # wrong thing).
          #
          # These methods also handle the deletion of each created
          # item after each example.
          define_singleton_method "with_#{type}" do |label|
            let(label.to_sym){new_item(type, :superuser)}
            after :each do
              delete_item(type, label)
            end
          end

          # +with_TYPEs+ methods accept a "splat" array of labels.
          # Thus, calling
          #
          #   with_actors :alice, :bob, :carol
          #
          # is equivalent to calling
          #
          #   with_actor :alice
          #   with_actor :bob
          #   with_actor :carol
          #
          # Note: the 1.9 "stabby lambda" notation allows splat arguments
          define_singleton_method "with_#{type}s", ->(*labels){
            labels.each do |label|
              self.public_send("with_#{type}", label)
            end
          }
        end

        # Asserts that a given HTTP verb is not allowed at a given API
        # endpoint.
        #
        # +method+ should be one of :GET, :PUT, :POST, or :DELETE
        #
        # +url_fragment+ should be a syntactically-legal URL path
        # (i.e. after the host and port).  The entities referenced in
        # the components of the fragment do not need to exist; this is
        # just enough information to ensure the routing gets the
        # request to the correct controller.  The existence of the
        # entities is checked after the validity of the HTTP verb is
        # determined.
        #
        # So, if you want to test that POST is not allowed on the
        # /actors/<actor_id> endpoint, call
        #
        #   should_not_allow_method :POST, "/actors/ffffffffffffffffffffffffffffffff"
        #
        def self.should_not_allow(method, url_fragment)
          context method do
            it "is not an allowed method" do
              response = self.public_send(method.downcase, url_fragment, :superuser)
              response.should have_status_code(405)
            end
          end
        end

        # Define all the ACE / ACL setting methods.
        #
        # Remember, they're all **context** methods, not **example**
        # methods.  They are only to be used for setting up test fixtures.
        [:actor, :group, :object, :container].each do |type|

          # with_ace_on_TYPE sets a single ACE on the +target+ object
          #
          # For instance, if you wish to add +:alice+ to the +actors+
          # list of the +DELETE+ ACE of actor +:bob+, you would invoke
          #
          #   with_ace_on_actor :bob, :delete, :actors => [:alice]
          #
          # Similar instructions apply to other Authz types, mutatis
          # mutandis.
          #
          # Note that these methods **set** the ACE; they do not
          # **append** to it.
          define_singleton_method "with_ace_on_#{type}" do |target, permission, ace|
            before :each do
              validate_entity_id(target)

              actors = (ace[:actors] || []).map{|n| resolve(n)}
              groups = (ace[:groups] || []).map{|n| resolve(n)}

              response = put("/#{type}s/#{resolve(target)}/acl/#{permission.downcase}",
                             :superuser,
                             :payload => {
                               "actors" => actors,
                               "groups" => groups
                             })
              response.should have_status_code(200)
            end
          end

          # with_acl_on_TYPE sets a complete ACL on a target item.
          #
          # It is equivalent to calling +with_ace_on_TYPE+ once for
          # each of the five Authz permissions.
          define_singleton_method "with_acl_on_#{type}" do |target, acl|
            [:create, :read, :update, :grant, :delete].each do |p|
              send("with_ace_on_#{type}", target, p, (acl[p] || {}))
            end
          end

        end

        def self.with_members(group, members={})
          before :each do

            group = resolve(group)

            actors = (members[:actors] || []).map{|n| resolve(n)}
            groups = (members[:groups] || []).map{|n| resolve(n)}

            actors.each do |a|
              r = put("/groups/#{group}/actors/#{a}", :superuser, :payload => nil)
              r.should have_status_code 200
            end

            groups.each do |g|
              r = put("/groups/#{group}/groups/#{g}", :superuser, :payload => nil)
              r.should have_status_code 200
            end
          end
        end

        def validate_entity_id(entity_id)
          unless resolve(entity_id) =~ /[0-9a-f]{32}/
            raise "Invalid entity id '#{entity_id}'! Should be a 32-character hex string"
          end
        end

        ################################################################################
        # Logging
        ################################################################################

        # If we're logging traffic, delimit the traffic from each test example
        if Pedant::Config.log_file
          before :each do
            File.open(Pedant::Config.log_file, 'a') do |f1|
              f1.puts("<-<-<-<-<-<-<-<")
              f1.puts("BEGIN: " + example.description)
              f1.puts
            end
          end

          after :each do
            File.open(Pedant::Config.log_file, 'a') do |f1|
              f1.puts
              f1.puts("END: " + example.description)
              f1.puts(">->->->->->->->")
            end
          end
        end

        # Execute an HTTP request against an Authz server
        #
        # `method` is an HTTP verb, as an uppercase symbol, e.g., :GET
        #
        # `url_fragment` is the URL path for the request
        #
        # `requestor` is the Authz ID of the entity on whose behalf this
        # request is being made.  It will become the value of the
        # "X-Ops-Requesting-Actor-Id" header for the request.
        #
        # `opts` is a hash of options that modify the request in some way.
        # The currently recognized keys and their effects are as follows:
        #
        # :headers => completely overrides the default headers for a
        # request.  If not supplied, standard headers are used.
        #
        # :merge_headers => merges the given headers with the default
        # ones.  Useful if you only need to change one or two headers.
        # A key of :DELETE will remove the header altogether.  If both
        # the +:headers+ and +:merge_headers options are present,
        # +:headers+ takes precedence.
        #
        # :payload => the body of the request.  This is required for all PUT
        # and POST requests.  It should be given in its final form (i.e., as
        # a String, not a Ruby hash or anything else)
        #
        # Finally, a block can be supplied to this method.  This block will
        # receive a single argument, the HTTP response (as a
        # RestClient::Response object).  Testing methods should use this to
        # carry out any validation tests of the response.
        def authz_request(method, url_fragment, requestor, opts={})

          url = api_url(url_fragment)

          requestor = resolve(requestor)

          default = {
            'Accept' => 'application/json',
            'Content-Type' => 'application/json',
            'User-Agent' => 'oc-authz-pedant',
            'X-Ops-Timestamp' => Time.now.utc.to_s,
            'X-Ops-User-Id' => 'front-end-service', # simulates oc_chef_authz, anyway
            'X-Ops-Requesting-Actor-Id' => requestor
          }

          headers = if opts[:merge_headers]
                      default.merge!(opts[:merge_headers]).reject{|k,v| v == :DELETE}
                    elsif opts[:headers]
                      opts[:headers]
                    else
                      default
                    end

          payload_raw = opts[:payload] || ""

          payload = if payload_raw.class == Hash
                      to_json(payload_raw)
                    else
                      payload_raw
                    end

          response_handler = lambda{|response, request, result| response}

          if [:PUT, :POST].include? method
            RestClient.send method.downcase, url, payload, headers, &response_handler
          else
            RestClient.send method.downcase, url, headers, &response_handler
          end
        end

        def get(url_fragment, req, opts={})
          authz_request(:GET, url_fragment, req, opts)
        end

        def put(url_fragment, req, opts={})
          authz_request(:PUT, url_fragment, req, opts)
        end

        def post(url_fragment, req, opts={})
          authz_request(:POST, url_fragment, req, opts)
        end

        def delete(url_fragment, req, opts={})
          authz_request(:DELETE, url_fragment, req, opts)
        end

        # We're using a lot of symbols to refer to things in RSpec let
        # blocks... this lets figure out what they refer to.
        def resolve(thing)
          case thing
          when Symbol
            self.public_send(thing)
          else
            thing
          end
        end

      end
    end
  end
end
