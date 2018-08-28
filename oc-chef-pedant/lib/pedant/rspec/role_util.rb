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

require 'pedant/request'
require 'rspec/core/shared_context'
require 'active_support/core_ext/hash/keys'
require 'pedant/concern'

module Pedant
  module RSpec
    module RoleUtil
      extend ::RSpec::Core::SharedContext
      extend ::Pedant::Concern

      # Add some context-level helper macros
      module ClassMethods

        # Call this to test several aspects of role creation.
        # Verifies proper status code and body for creation request,
        # as well as the fact that the created role can be retrieved.
        #
        # Creates several `it` blocks; should be called in a context
        # that has values for `request_method` (must be POST),
        # `request_url` (must be to the /roles endpoint),
        # `request_payload`, and `requestor`, as well as `role_name`
        def should_successfully_create_a_role(message=nil)
          it 'should respond with 201 and the correct path' do
            request_method.should == :POST
            request_url.should == api_url("/roles")
            should look_like create_role_success_response
          end
          it "should persist the role#{message ? ' ' + message : ''}" do
            # Extra paranoia here
            request_payload['name'].should == role_name
            response.should look_like created_response # this saves it again
            get(api_url("/roles/#{role_name}"), requestor).should look_like fetch_role_success_response
          end
        end

        # Asserts that creating a role should not succeed.  Verifies
        # the status code and error message for the failed creation
        # attempt, as well as the fact that no role is actually
        # created.
        #
        # Creates several `it` blocks; should be called in a context
        # that has values for `request_method` (must be POST),
        # `request_url` (must be to the /roles endpoint),
        # `request_payload`, and `requestor`, as well as `role_name`
        def should_fail_to_create_a_role(code, message)
          # This checks the status code and error message of the failed creation attempt
          it{
            request_method.should == :POST
            request_url.should == api_url('/roles')
            should have_error code, message
          }
          it 'should not actually create a role' do
            # This attempts the creation again (this is a separate `it` block)
            response.should look_like bad_request_response
            begin
              get(api_url("/roles/#{role_name}"), requestor).should look_like resource_not_found_response
            rescue URI::InvalidURIError
              # Means we tried to create with a bogus, non-URL-safe
              # role_name; this is OK
            end
          end
        end

        # Asserts that role deletion should succeed.  Verifies proper
        # status code and body for the deletion request, as well as
        # the fact that the deleted role cannot be subsequently
        # retrieved.
        #
        # Creates several `it` blocks; should be called in a context
        # that has values for `request_method` (must be DELETE),
        # `request_url` (must be to the /roles endpoint), and
        # `requestor`, as well as `role_name`
        def should_successfully_delete_a_role
          it 'should respond with 201 and the deleted role body' do
            request_method.should == :DELETE
            request_url.should == api_url("/roles/#{role_name}")
            response.should look_like delete_role_success_response
          end
          it 'should actually delete the role' do
            # This deletes the role again (this is a separate `it` block)
            response.should look_like ok_response
            get(api_url("/roles/#{role_name}"), requestor).should look_like role_not_found_response
          end
        end

        # Asserts that a PUT request to a given role should fail.
        # Verifies proper status code and error message for the failed
        # update attempt, as well as the fact that the role in
        # question is not changed in any way.
        #
        # Creates several `it` blocks; should be called in a context
        # that has values for `request_method` (must be PUT),
        # `request_url` (must be to the /roles/<role> endpoint),
        # `request_payload`, and `requestor`, as well as `role_name`
        def should_fail_to_update_a_role(code, message)
          # This verifies that the update request fails as expected
          it{
            request_method.should == :PUT
            request_url.should == api_url("/roles/#{role_name}")
            response.should have_error code, message
          }
          # Compare the role before and after the failed update to
          # ensure nothing changed
          it 'does not change the role in any way' do
            original = parse(get(request_url, requestor))
            # This performs the ill-fated update request
            response.should look_like bad_request_response
            post_request = parse(get(request_url, requestor))
            original.should == post_request
          end
        end

        # Asserts that a PUT request to a given role should succeed.
        # Verifies proper status code and response body for the PUT
        # request, as well as verifies that the role is changed in all
        # the appropriate ways.
        #
        # Creates several `it` blocks; should be called in a context
        # that has values for `request_method` (must be PUT),
        # `request_url` (must be to the /roles/<role> endpoint),
        # `request_payload`, and `requestor`.
        #
        # Additionally, an `updated_fields` hash should be defined,
        # detailing which fields of the role are going to be altered.
        # This influences the comparison tests that will be made
        # comparing the original and updated roles, in order to verify
        # that the correct fields (and only those fields) were
        # updated.
        def should_successfully_update_a_role
          it 'should respond with 200 and the updated role body' do
            request_method.should == :PUT
            request_url.should == api_url("/roles/#{role_name}")
            response.should look_like update_role_success_response
          end
          it 'should actually update the role' do
            # check the value before
            original = parse(get(request_url, requestor))
            # update the role
            response.should look_like ok_response
            # retrieve the new value
            updated = parse(get(request_url, requestor))

            # Now, compare to earlier value w/r/t updated_fields

            # All the updated fields should be changed
            should_be_hash(updated_fields)
            updated_fields.each do |field, value|
              if value == :DELETE
                # With our update helper function, a value of :DELETE
                # indicates that the field should not be present in
                # the PUT request payload (note that this is a feature
                # of this testing framework, and not of the API per
                # se).  As such, it shouldn't get modified; the value
                # of the original and updated roles should be
                # identical.
                original[field].should == updated[field]
              else
                # On the other hand, if the value isn't :DELETE, then
                # we actually should have changed something.
                original[field].should_not == value
                updated[field].should == value
              end
            end

            # None of the non-updated fields should be changed
            common_fields = original.keys - updated_fields.keys
            common_fields.each do |field|
              original[field].should == updated[field]
            end
          end
        end

      end # ClassMethods

      # When you include this context, 'role_name' is set to the name
      # of the testing role, and 'role' is set to the Ruby Hash of the
      # actual role
      shared_context 'with temporary testing role' do
        let(:role_name){unique_name("temporary_role")}
        let(:role_description){"blah"}
        let(:role_override_attributes){ {} }
        let(:role_default_attributes){ {} }
        let(:role_run_list){ [] }
        let(:role_env_run_lists){ {} }
        let(:role_payload) {{
            'name' => role_name,
            'description' => role_description,
            'json_class' => "Chef::Role",
            'chef_type' => 'role',
            'default_attributes' => role_default_attributes,
            'override_attributes' => role_override_attributes,
            'run_list' => role_run_list,
            'env_run_lists' => role_env_run_lists
          }}
        let(:role){role_payload}

        before :each do
          add_role(admin_requestor, role_payload)
        end

        after :each do
          delete_role(admin_requestor, role_name)
        end
      end # shared context

      # Override as needed
      # let(:create_role_as_non_admin_response) { create_role_success_response }
      #let(:update_role_as_non_admin_response) { update_role_success_response }
      #let(:delete_role_as_non_admin_response) { delete_role_success_response }

      let(:fetch_role_success_response) do
        {
          :status => 200,
          :body => normalize_role(role)
        }
      end

      let(:fetch_roles_list_response) do
        {
          status: 200,
          :body => roles
        }
      end

      let(:create_role_success_response) do
        {
          :status => 201,
          :body => { "uri" => api_url("/roles/#{role_name}") }
        }
      end

      let(:create_role_conflict_response) do
        {
          :status => 409,
          :body => { "error" => ["Role already exists"] }
        }
      end

      let(:update_role_success_response) do
        {
          :status => 200,
          :body => updated_role
        }
      end

      let(:delete_role_success_response) do
        {
          :status => 200,
          :body => role
        }
      end

      let(:role_not_found_response) do
        {
          :status => 404,
          :body => { "error" => ["Cannot load role #{role_name}"] }
        }
      end

      let(:invalid_role_response) do
        {
          :status => 400,
          :body =>   { "error" => invalid_role_error_message }
        }
      end

      let(:invalid_role_error_message) { fail "Define let(:invalid_role_error_message) in the example group" }

      # Ensure that all run lists in a role have been normalized.
      def normalize_role(role)
        run_list = role['run_list'] || []
        role['run_list'] = normalize_run_list(run_list)

        env_run_lists = role['env_run_lists'] || {}
        normalized = env_run_lists.inject({}){|acc, item|
          env = item.first
          list = item.last
          acc[env] = normalize_run_list(list)
          acc
        }
        role['env_run_lists'] = normalized
        role
      end

      def add_role(requestor, role)
        post(api_url("/roles"),
             requestor,
             :payload => role)
      end

      def delete_role(requestor, name)
        begin
          delete(api_url("/roles/#{name}"),
                 requestor)
        rescue URI::InvalidURIError
          # OK, don't fail
        end
      end

      def new_role(name, opts = {}, lists = {} )
        {
          "name" => name,
          "description" => "blah",
          "json_class" => "Chef::Role",
          "default_attributes" => {},
          "override_attributes" => {},
          "chef_type" => "role",
          "run_list" =>  [],
          "env_run_lists" => lists[:env_run_lists] || {}
        }.merge(opts.stringify_keys).merge(lists.stringify_keys)
        # TODO: For backwards compatibility. This should be simplified to arity 1 and the tests refactored
      end
    end
  end
end
