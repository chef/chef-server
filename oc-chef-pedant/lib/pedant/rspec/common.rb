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

require 'rspec-shared/methods'

require 'pedant/concern'
require 'pedant/json'
require 'pedant/request'
require 'pedant/rspec/common_responses'
require 'pedant/rspec/http_status_codes'

# Temporary, until knife tests are activated for non-open-source platforms

module Pedant
  module RSpec
    module Common

      extend Pedant::Concern
      # We use a concern instead of a shared context in order to access
      # the complete Rspec DSL, plus extensions (metadata, shared(), etc.)

      included do |base|

        base.extend(RSpecShared::Methods)
        include Pedant::JSON
        include Pedant::Request
        include Pedant::RSpec::CommonResponses


        # Request/Response
        subject { response } # By default, we are always testing responses
        let(:response) { authenticated_request(request_method, request_url_with_query_parameters, requestor, request_options.merge({server_api_version: request_version })) }
        let(:request_url_with_query_parameters){ request_url + (request_query_parameters ? "?#{request_query_parameters}" : "") }
        let(:parsed_response) { parse(response) }
        let(:request_version) { server_api_version }
        let(:request_method) { fail "Define one of the following: :GET, :POST, :PUT, :DELETE" }
        let(:requestor)      { fail "Define requestor (ex: admin_user, normal_user, etc.)" }
        let(:request_url)    { fail "Define url" }

        let(:request_options) { { payload: request_payload, headers: request_headers, auth_headers: request_auth_headers } }
        let(:request_payload) { nil }      # No payload by default
        let(:request_headers) { {} }       # No extra headers by default
        let(:request_auth_headers) { nil } # Override for your own headers
        let(:request_query_parameters){ nil } # This should be a string, like "foo=bar"

        # Pedant-created requestors:
        let(:pedant_clients) { ([ platform.validator_client_name ] + pedant_created_clients).sort }
        let(:pedant_created_clients) { platform.clients.reject(&:bogus?).map(&:name).sort }
        let(:pedant_users)   { (['admin'] + platform.users.map(&:name)).sort }


        def self.named_response_code(code)
          "#{code} #{Pedant::RSpec::HTTP::STATUS_CODES[code]}"
        end

        # TODO: With request query parameters, in order to be completely
        # general, we would first need to differentiate between query
        # parameters intended for the search endpoint and otherwise in
        # order to escape any embedded Solr query operators, and then
        # URI encode the whole thing.
        def self.should_respond_with(code, additional_message = nil, metadata = {}, &additional_assertions)
          metadata[:validation] = true if code == 400
          metadata[:authentication] = true if code == 401
          metadata[:authorization] = true if code == 403
          it ["should respond with", named_response_code(code), additional_message].compact.join(' '), metadata do
            should look_like expected_response

            instance_eval(&additional_assertions) if additional_assertions
          end
        end

        def self.should_respond_with_error(code, message)
          it "should respond with #{named_response_code(code)}" do
            should have_error code, message
          end
        end

        # When creating a Chef object, not all of the fields we accept
        # in JSON request bodies need to be present; if missing, the
        # Chef Server will set them to sensible default values.
        #
        # This should be used in a context that has values already set
        # for request_url, request_method (assumed to be POST, or PUT
        # for cookbooks), request_payload, and requestor, all of which
        # govern the object creation request.
        #
        # Additionally, a 'fetched_resource_response' should be defined,
        # which should be a request to retrieve the created object.
        # Note that this is only called after object creation.
        #
        # 'object_type' should be the singular name of the kind of
        # thing you're creating (e.g., 'role', 'node', 'environment',
        # etc.) (Though this is only used in RSpec context naming, so
        # the world will not end if you pass something slightly wrong.)
        #
        # 'field' is the field in the request payload JSON that we'll be
        # manipulating.  Currently, this should be 'chef_type' or
        # 'json_class', though this isn't enforced.
        #
        # 'proper_value' should be the, well, proper value that
        # 'field' should have for the kind of thing you're trying to
        # create (e.g., 'role' for 'chef_type', 'Chef::Role' for
        # 'json_class', etc.)
        def self.should_set_default_value_for(object_type, field, default_value)
          context "when missing '#{field}' value" do
            it  "should create #{object_type} and set '#{default_value}' as '#{field}' value" do
              # Ensure that the field-under-test is not present in the request payload
              request_payload.delete field

              # This actually does the create, according to the 'request_method',
              # 'request_url', and 'request_payload'
              response.should look_like created_response

              # Retrieve the thing we created and verify it has the
              # proper value for the field.  Note that we are only
              # looking at the field, not the rest of the body; other
              # tests do that.
              fetched_resource_response.should look_like({
                                                           :status => 200,
                                                           :body => {
                                                             field => default_value
                                                           }
                                                         })
            end
          end
        end

        # When creating a Chef object, test how missing and incorrect
        # values of type metadata in the payload value affect object
        # creation.  In general, these are data that have only one
        # acceptable value (e.g., chef_type or json_class).
        #
        # See the documentation for
        # `self.should_set_default_value_for_object` for parameter
        # descriptions and caveats; they are the same.
        def self.test_create_behavior_of_type_metadata(object_type, field, proper_value)
          should_set_default_value_for(object_type, field, proper_value)

          context "when using something other than '#{proper_value}' as '#{field}' value" do
            it  "should fail to create the #{object_type}" do

              # Ensure that the value in the payload will always be wrong.
              override_value = "gibberish_that_will_never_be_a_valid_value"
              override_value.should_not == proper_value
              request_payload[field] = override_value

              # Try to create the object, and fail miserably
              response.should look_like({
                                          :status => 400,
                                          :body_exact => {"error" => ["Field '#{field}' invalid"]}
                                        })
            end
          end
        end

        # When attempting to create a Chef object, if data are
        # provided that are not of the correct type (e.g., hash,
        # array, string, etc.), creation should fail.
        #
        # 'proper_type' should be a string naming the expected valid
        # data type for the given field (e.g., 'hash').  This is used
        # for test descriptions, as well as in validation of the error
        # message that should come back in the response.
        #
        # 'test_value' is the incorrect data that will be inserted
        # into the request_payload to trigger the desired error.  As
        # an example, if a given field should be a hash, set
        # 'test_value' to the string "this is not a hash!".
        #
        # See the
        # documentation for `self.should_set_default_value_for_object`
        # for other parameter descriptions and caveats; they are the same.
        def self.should_not_allow_creation_with_incorrect_types(object_type, field, proper_type, test_value)
          context "with a non-#{proper_type} value for '#{field}'", :validation do
            it "fails to create the #{object_type}" do
              request_payload[field] = test_value
              response.should look_like({
                                          :status => 400,
                                          :body_exact => {
                                            "error" => ["Field '#{field}' is not a #{proper_type}"]
                                          }
                                        })
            end
          end
        end

        # Test the behavior of endpoints that accept payloads when the
        # payload exceeds the maximum allowable size.
        #
        # Should be used in a context that has 'request_method' (works
        # for both POST and PUT!), 'request_url', and 'requestor'
        # variables set.
        #
        # These tests use large JSON data structures... they are legal
        # JSON, but not valid JSON for any particular endpoint (this
        # way it can be reused for other endpoint tests).  This is OK,
        # though, because the focus of the tests is whether or not the
        # endpoints return the proper HTTP status code (413)
        #
        # We're only testing whether the size is a problem or not.
        def self.respects_maximum_payload_size
          context 'with a payload size', :validation do
            let(:maximum_request_size){1000000}
            context 'exactly equal to the maximum allowable size' do
              let(:request_payload) { Pedant::Utility.get_pedant_file("payloads/maxfile.json").read }
              # We use this form of `it` because the
              # `have_status_code` prints a nice message, which we'll
              # use for the name of this test
              it {
                request_payload.length.should == maximum_request_size
                # For this test, we don't really care what the
                # response is; just that it's *not* a 413.
                should_not have_status_code 413
              }
            end
            context 'exceeding the maximum allowable size' do
              let(:request_payload) { Pedant::Utility.get_pedant_file("payloads/toobigfile.json").read }
              it{
                request_payload.length.should be > maximum_request_size
                # Again, not particularly caring what the body is,
                # just that the code is correct
                should have_status_code 413
              }
            end
          end
        end

        # For objects that have run lists (i.e., nodes and roles),
        # generate tests for some quirky corner cases.
        #
        # Must be run in a context that has `request_method` = :POST,
        # `request_url` set to either api_url("/nodes") or
        # api_url("/roles"), and either `node_name` or `role_name` set
        # to an appropriate value.
        def self.test_run_list_corner_cases(object_type)
          valid_args = [:node, :role]
          if !valid_args.include? object_type
            raise "Can only perform run list tests for one of #{valid_args}!  You tried '#{object_type}'"
          end

          # Either :node or :role, to create the 'let' block of the same name
          object_symbol = object_type

          # Either :node_name or :role_name; expected to be set in containing context
          object_name_symbol = "#{object_type}_name".to_sym

          # Method to invoke that actually generates the tests
          test_method_symbol = "should_successfully_create_a_#{object_type}".to_sym

          # Method to invoke to create the test object
          object_constructor_symbol = "new_#{object_type}".to_sym

          context 'with a non-normalized run list' do
            let(:run_list) do
              ["foo", "foo::bar", "bar::baz@1.0.0", "recipe[web]", "role[prod]"]
            end
            let(object_symbol){send(object_constructor_symbol, send(object_name_symbol), :run_list => run_list)}
            send(test_method_symbol, "with a normalized run list")
          end

          context "with a runlist that has recipes named 'recipe' and 'role'" do
            let(:run_list) do
              # This is pretty bizarre, and we don't expect to see things
              # like this in real life, but they should be legal.
              ["recipe", "recipe::foo", "recipe::bar@1.0.0", "role", "role::foo", "role::bar@1.0.0",
               "recipe[recipe]", "recipe[role]", "role[recipe]", "role[role]"]
            end
            let(object_symbol){send(object_constructor_symbol, send(object_name_symbol), :run_list => run_list)}
            send(test_method_symbol, "with all oddly-named recipes intact in the run list")
          end

          context "with a runlist that has duplicate recipes and roles" do
            let(:run_list) do
              ["webserver", "recipe[webserver]", "role[prod]", "role[prod]"]
            end
            let(object_symbol){send(object_constructor_symbol, send(object_name_symbol), :run_list => run_list)}
            send(test_method_symbol, "with all run list duplicates removed")
          end

          context "with a runlist that has implicit and explict 'default' recipes" do
            let(:run_list) do
              ["webserver", "webserver::default"]
            end
            let(object_symbol){send(object_constructor_symbol, send(object_name_symbol), :run_list => run_list)}
            send(test_method_symbol, "with both versions remaining in the run list")
          end

        end # test_run_list_corner_cases

        # Timestamp suffixes
        # Suffix unique between runs. Timestamp is generated once per pedant run
        shared(:pedant_suffix) { suffix_for_names.(platform.pedant_run_timestamp) }

        # Suffix unique to the example, bound to a let().
        shared(:unique_suffix) { suffix_for_names.(platform.timestamp) }
        shared(:suffix_for_names) {->(t) { "#{t.to_i}-#{t.nsec}-#{Process.pid}" } }

        # The suffix is unique for Pedant runs, but not within each pedant run.
        # For a unique timestamp within a Pedant run, use platform.timestamp
        # or unique_suffix
        #
        # - Prefixes "pedant_" for the name
        # - Appends a suffix unique for the entire Pedant run
        # - Useful for things like names for roles, nodes, etc.
        def unique_name(name)
          "pedant_#{name}_#{pedant_suffix}"
        end

        # Helper method for helper methods; useful for ensuring that
        # parameters are the correct data type
        def should_be_hash(data)
          if data.class != Hash
            raise "Data needs to be a hash, you passed in a #{data.class}: #{data}"
          end
        end

        # Helper method for helper methods; useful for ensuring that
        # parameters are the correct data type
        def should_be_string(data)
          if data.class != String
            raise "Data needs to be a string; you passed in a #{data.class}: #{data}"
          end
        end

        # Given a Ruby hash representation of a Chef object and a hash
        # of updated field -> value pairs, merges the updates into the
        # object, returning the updated object.  Any updated field
        # with a value of :DELETE will be removed from the updated
        # object.
        def update_object(object, updates)
          # Verify types
          should_be_hash(object)
          should_be_hash(updates)

          # Pull out all update fields that are targeted for deletion
          fields_to_delete = updates.inject([]) do |acc, element|
            field = element.first
            value = element.last
            value == :DELETE ? acc.push(field) : acc
          end

          # Perform the initial merge.  Fields targeted for deletion are still present.
          updated_object = object.merge(updates)

          # Remove all fields targeted for deletion, if any
          fields_to_delete.each do |field|
            updated_object.delete(field)
          end

          # We're done here
          updated_object
        end

        # Ensure that all elements of a run list are explicitly tagged
        # as either a recipe or a role.  Also filters out duplicates
        # once everything has been normalized.
        def normalize_run_list(run_list)
          run_list.map{|item|
            case item
            when /^recipe\[.*\]$/
              item # explicit recipe
            when /^role\[.*\]$/
              item # explicit role
            else
              "recipe[#{item}]"
            end
          }.uniq
        end

        # Use this to wrap a group of examples into a focused context
        # We can't use focus because Rspec already has that directive
        def self.isolate(message = nil, &examples)
          context message || "when isolated", :focus do
            instance_eval(&examples)
          end
        end

        # Used to easily documented endpoints that don't have tests
        def self.untested_endpoint(endpoint_name, reason)
          context endpoint_name do
            it "returns success" do
              pending(reason)
              raise NotImplementedError
            end
          end
        end

        # Run the given block once (using 'key' to identify
        # the block).  This allows us to do "before :each"
        # cleanup to clean up from previous test runs, since
        # before :all causes the world to die.
        def once_per_run(key, &block)
          Pedant::OncePerRun.once_per_run(key, &block)
        end

        def platform
          Pedant::Config.pedant_platform
        end


        ## TODO: Remove this method; we probably don't need to access it directly
        def server
          platform.server
        end

        # construct a complete API URL based on the pre-configured server information
        def api_url(path_fragment)
          platform.api_url(path_fragment)
        end

        # Given a response object, verify the HTTP status is in the
        # 200-ish success range and raise an error if it is not. This
        # is intended to be used in helper/util modules where we want
        # to ensure the success of setup and teardown operations.
        def ensure_2xx(response)
          if response.code > 299
            raise "bad response code #{response.code} in response: #{response}"
          end
          response
        end

        ################################################################################
        # Logging
        ################################################################################

        # If we're logging traffic, delimit the traffic from each test example
        if Pedant::Config.log_file
          before :each do |example|
            File.open(Pedant::Config.log_file, 'a') do |f1|
              f1.puts("<-<-<-<-<-<-<-<")
              f1.puts("BEGIN: " + example.description)
              f1.puts
            end
          end

          after :each do |example|
            File.open(Pedant::Config.log_file, 'a') do |f1|
              f1.puts
              f1.puts("END: " + example.description)
              f1.puts(">->->->->->->->")
            end
          end
        end

        ################################################################################
        # Miscellaneous Helper Functions
        ################################################################################

        # Helper function for setting options; return option is set, otherwise return default
        # Prevents repetitive logic
        def set_opt(opt, default)
          if opt
            opt
          else
            default
          end
        end

        ################################################################################
        # Users, Clients, and Organizations
        ################################################################################

        # If these are referenced in before(:all) blocks, use shared() instead of let()
        shared(:organization)     { platform.test_org }
        shared(:org)              { platform.test_org.name }
        # TODO look at how these are set up - is accurate?
        shared(:admin_user)       { platform.admin_user }
        shared(:org_admin)        { platform.admin_user}
        shared(:normal_user)      { platform.non_admin_user }

        shared(:outside_user)     { platform.bad_user}

        # TODO no such thing - eliminate tests referring to it!
        shared(:admin_client)     { platform.admin_client }
        # TODO all non-validator clients are normal clients.
        shared(:normal_client)    { platform.non_admin_client }
        shared(:outside_client)   { platform.bad_client }
        shared(:validator_client) { platform.validator_client }

        shared(:knife_admin)      {  admin_user }
        shared(:knife_user)       { normal_user}

        # TODO: Ultimately, I'd like to see all access to the superuser go
        # away, and all tasks that require its use become methods on the
        # Platform object

        shared(:superuser) { platform.superuser }
        shared(:superuser_key) { platform.superuser_key }
        shared(:webui_key) { platform.webui_key }

        # Given a requestor, create a new one with the same name, but
        # with the web UI's private key for 'impersonation' tests
        def impersonate(requestor_to_impersonate)
          Pedant::Requestor.new(requestor_to_impersonate.name, platform.webui_key)
        end

        # Need a well-formed yet invalid key for a requestor to test authentiction
        shared(:bogus_key) { platform.bogus_key }
        shared(:invalid_user) { Pedant::Requestor.new('invalid', bogus_key, bogus: true) }

        ################################################################################
        # Test Context Helpers
        ################################################################################

        # We have helper functions for adding and removing Chef objects from
        # the system in before and after blocks (like `add_role` \
        # `delete_role` in the `roles_util.rb` file).  This is a
        # generalization of the pattern for use in shared examples, achieved
        # by parameterizing on the container name.

        def add_chef_object(container_name, requestor, object_json)
          post(api_url("/#{container_name}"), requestor, :payload => object_json)
        end

        def delete_chef_object(container_name, requestor, object_name)
          delete(api_url("/#{container_name}/#{object_name}"), requestor)
        end

        # DSL helpers
        def instance_eval_if_proc(object)
          Proc === object ? instance_eval(&object) : object
        end

        # Debugging
        # Example: somevalue.tap(&watch)
        let(:watch) { ->(x) { puts x } }
      end
    end
  end
end
