# -*- coding: utf-8 -*-
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

require 'pedant/rspec/auth_headers_util'
require 'pedant/rspec/environment_util'

describe "Environments API Endpoint", :environments do
  include Pedant::RSpec::EnvironmentUtil
  include Pedant::RSpec::AuthHeadersUtil

  # Deal with subtly different error messages/codes in one place
  let(:new_environment_name) { 'pedant_testing_environment' }
  let(:non_existent_environment_name) { 'pedant_dummy_environment' }

  let(:requestor) { admin_user }

  context 'with no additional environments' do

    before(:each) { delete_environment(admin_user, new_environment_name) }
    after(:each) { delete_environment(admin_user, new_environment_name) }


    context 'PUT /environments' do
      let(:request_method) { :PUT }
      let(:request_url)    { api_url '/environments' }

      let(:expected_response) { method_not_allowed_response }
      should_respond_with 405
    end # PUT /environments

    context "PUT /environments/<name>" do
      let(:request_method)  { :PUT }
      let(:request_url)     { api_url "/environments/#{environment_name}" }
      let(:request_payload) { full_environment(environment_name) }

      context 'with a non-existent environment' do
        let(:environment_name) { non_existent_environment_name }
        let(:expected_response) { resource_not_found_exact_response }
        let(:not_found_error_message) { cannot_load_nonexistent_env_msg }

        should_respond_with 404
      end

      context 'when updating the "_default" environment' do
        let(:environment_name) { '_default' }
        let(:expected_response) { method_not_allowed_exact_response }
        let(:error_message) { ["The '_default' environment cannot be modified."] }

        should_respond_with 404
      end
    end

  end

  context 'with non-default environments in the organization' do

    before(:each) { add_environment(admin_user, full_environment(new_environment_name)) }
    after(:each)  { delete_environment(admin_user, new_environment_name) }

    describe 'PUT /environments/<name>' do
      let(:request_method) { :PUT }
      let(:request_url)    { api_url "/environments/#{environment_name}" }
      let(:request_payload) { new_environment(environment_name) }
      let(:environment_name) { new_environment_name }

      respects_maximum_payload_size

      context 'with a valid update', :smoke  do
        it { should look_like ok_response }
      end

      context 'when handling request headers and payload' do
        include_context "environment_body_util"

        context 'with authentication headers' do
          # Unconverted Auth Headher spec DSL
          let(:method) { request_method }
          let(:url)    { request_url }
          let(:body)   { request_payload }

          let(:response_should_be_successful) do
            response_body = empty_payload
            response_body['name'] = new_environment_name
            response.should look_like({
              :status => 200,
              :body_exact => response_body
            })
          end
          let(:success_user) { admin_user }
          let(:failure_user) { outside_user }

          include_context 'handles authentication headers correctly'
        end

        context 'when unable to accept application/json' do
          let(:request_headers) { { "Accept" => "application/xml" } }
          let(:expected_response) { not_acceptable_response }

          should_respond_with 406
        end

        context 'when sending something other than application/json' do
          let(:request_headers) { { "Content-Type" => "application/xml" } }
          let(:request_payload) { '<environment name="blah"></environment>' }

          skip "webmachine not sucking" do
            should_respond_with 415
          end
        end

        context 'with unparsable JSON payload', :validation do
          let(:request_payload) { '{"hi' }
          let(:expected_response) { bad_request_response }

          should_respond_with 400
        end

        context 'with empty request payload', :validation do
          let(:request_payload) { '' }
          let(:expected_response) { bad_request_response }

          should_respond_with 400
        end

      end

      context 'when validating', :pedantic do
        include_context "environment_body_util"

        context 'with invalid body' do
          def self.should_respond_with_bad_request(message, _payload)
            context message, :validation do
              let(:request_payload) { _payload }
              should_respond_with_error 400, incorrect_json_type_body_msg
            end
          end

          context 'with an array', :validation do
            let(:request_payload) { '["name","blah"]' }

            should_respond_with_error 400, incorrect_json_type_body_msg
          end

          should_respond_with_bad_request 'with a string', '"environment"'
          should_respond_with_bad_request 'with a number', '-1'
          should_respond_with_bad_request 'with a floating number', '9.9'
          should_respond_with_bad_request 'with a boolean', 'true'
          should_respond_with_bad_request 'with a null', 'null'

          fails_with_value("blah", "anyoldvalue", "Invalid key blah in request body", true)
        end

        context 'for name' do
          fails_with_value("name", :delete, "Field 'name' missing", true)
          fails_with_value("name", "",
                           "Field 'name' invalid", true, false)
          fails_with_value("name", "abc!123",
                           "Field 'name' invalid", true, false)
          fails_with_value("name", "abc 123",
                           "Field 'name' invalid", true, false)

          # TODO: Looks like we're losing the null character with the
          # ejson:decode call which is eating it silently
          # OSC is not using ejson anymore.
          # Test works on OSC, but unknown if it works on the other platforms
          # TODO find out if this is actually failing....
          context 'ejson:decode eats nulls' do
            fails_with_value("name", "abc\u0000123",
                             "Field 'name' invalid", true)
          end

          fails_with_value("name", "大爆発",
                           "Field 'name' invalid", true, false)
          fails_with_value("name", nil, incorrect_json_type_name_msg, true, false)
          fails_with_value("name", 1999, incorrect_json_type_name_msg, true, false)
          fails_with_value("name", true, incorrect_json_type_name_msg, true, false)
          fails_with_value("name", [], incorrect_json_type_name_msg,true, false)
          fails_with_value("name", {}, incorrect_json_type_name_msg, true, false)
        end

        context "for description" do
          succeeds_with_value("description", :delete, nil, true)
          succeeds_with_value("description", "", nil, true)
          succeeds_with_value("description", "normal text", nil, true)
          succeeds_with_value("description", "これは日本語だ", nil, true)
          succeeds_with_value("description",
                              "\u0048\u0065\u006c\u006c\u006f\u0020\u65e5\u672c!",
                              nil, true)

          fails_with_value("description", 1999, "Field 'description' invalid", true, false)
        end

        context "for json_class" do
          succeeds_with_value("json_class", "Chef::Environment", nil, true)

          succeeds_with_value("json_class", :delete, nil, true)
          fails_with_value("json_class", "", incorrect_json_type_class_msg,
                           true)
          fails_with_value("json_class", 1999, incorrect_json_type_class_msg, true, false)
          fails_with_value("json_class", "notaclass",
                           incorrect_json_type_class_msg, true, false)
        end

        context "for chef_type" do
          succeeds_with_value("chef_type", "environment", nil, true)
          succeeds_with_value("chef_type", :delete, nil, true)

          fails_with_value("chef_type", "", incorrect_json_type_type_msg, true)
          fails_with_value("chef_type", 1999, incorrect_json_type_type_msg, true)
          fails_with_value("chef_type", "notaclass", incorrect_json_type_type_msg, true)
        end

        context "for default_attributes" do
          succeeds_with_value("default_attributes", :delete, nil, true)
          succeeds_with_value("default_attributes", {}, nil, true)
          succeeds_with_value("default_attributes", {"key1" => "value1",
                                "key2" => "value2"}, nil, true)

          fails_with_value("default_attributes", 1999,
                           "Field 'default_attributes' is not a hash", true, false)
          fails_with_value("default_attributes", "hello",
                           "Field 'default_attributes' is not a hash", true, false)

          context "for keys" do
            succeeds_with_value("default_attributes", {"key" => "value"}, nil, true)
            succeeds_with_value("default_attributes", {"鍵" => "value"}, nil, true)
            succeeds_with_value("default_attributes", {"" => "value"}, nil, true)

            fails_with_value("default_attributes", '{null:"value"}',
                             "Field 'default_attributes' is not a hash", true, false)
            fails_with_value("default_attributes", '{99:"value"}',
                             "Field 'default_attributes' is not a hash", true, false)
            fails_with_value("default_attributes", '{"key":"value","key":"value"}',
                             "Field 'default_attributes' is not a hash", true, false)
          end

          context "for values" do
            succeeds_with_value("default_attributes", {"key" => ""}, nil, true)
            succeeds_with_value("default_attributes", {"key" => nil}, nil, true)
            succeeds_with_value("default_attributes", {"key" => 99}, nil, true)
            succeeds_with_value("default_attributes", {"key" => "値"}, nil, true)
          end
        end

        context "for override_attributes" do
          succeeds_with_value("override_attributes", :delete, nil, true)
          succeeds_with_value("override_attributes", {}, nil, true)
          succeeds_with_value("override_attributes", {"key1" => "value1",
                                "key2" => "value2"}, nil, true)

          fails_with_value("override_attributes", 1999,
                           "Field 'override_attributes' is not a hash", true, false)
          fails_with_value("override_attributes", "hello",
                           "Field 'override_attributes' is not a hash", true, false)

          context "for keys" do
            succeeds_with_value("override_attributes", {"key" => "value"}, nil, true)
            succeeds_with_value("override_attributes", {"鍵" => "value"}, nil, true)
            succeeds_with_value("override_attributes", {"" => "value"}, nil, true)
            fails_with_value("override_attributes", '{null:"value"}',
                             "Field 'override_attributes' is not a hash", true, false)
            fails_with_value("override_attributes", '{99:"value"}',
                             "Field 'override_attributes' is not a hash", true, false)
            fails_with_value("override_attributes", '{"key":"value","key":"value"}',
                             "Field 'override_attributes' is not a hash", true, false)
          end

          context "for values" do
            succeeds_with_value("override_attributes", {"key" => ""}, nil, true)
            succeeds_with_value("override_attributes", {"key" => nil}, nil, true)
            succeeds_with_value("override_attributes", {"key" => 99}, nil, true)
            succeeds_with_value("override_attributes", {"key" => "値"}, nil, true)
          end
        end

        context "for cookbooks" do
          succeeds_with_value("cookbook_versions", :delete, nil, true)
          succeeds_with_value("cookbook_versions", {}, nil, true)

          fails_with_value("cookbook_versions", 1999,
                           "Field 'cookbook_versions' is not a hash", true, false)
          fails_with_value("cookbook_versions", "hello",
                           "Field 'cookbook_versions' is not a hash", true, false)

          context "for cookbook names" do
            succeeds_with_value("cookbook_versions", {"cookbook" => ">= 1.0.0"}, nil,
                                true)
            fails_with_value("cookbook_versions", {"the cookbook" => ">= 1.0.0"},
                             "Invalid key 'the cookbook' for cookbook_versions", true, false)
            fails_with_value("cookbook_versions", {"料理書" => ">= 1.0.0"},
                             "Invalid key '料理書' for cookbook_versions", true, false)
            fails_with_value("cookbook_versions", {"" => ">= 1.0.0"},
                             "Invalid key '' for cookbook_versions", true, false)
            fails_with_value("cookbook_versions", '{null:">= 1.0.0"}',
                             "Field 'cookbook_versions' is not a hash", true, false)
            fails_with_value("cookbook_versions", '{1999:">= 1.0.0"}',
                             "Field 'cookbook_versions' is not a hash", true, false)
          end

          context "for versions" do
            succeeds_with_value("cookbook_versions", {"cookbook" => ">= 1.0.0"}, nil, true)
            succeeds_with_value("cookbook_versions", {"cookbook" => ">= 1.0"}, nil, true)
            succeeds_with_value("cookbook_versions", {"cookbook" => "<= 1.0.0"}, nil, true)
            succeeds_with_value("cookbook_versions", {"cookbook" => "> 1.0.0"}, nil, true)
            succeeds_with_value("cookbook_versions", {"cookbook" => "< 1.0.0"}, nil, true)
            succeeds_with_value("cookbook_versions", {"cookbook" => "= 1.0.0"}, nil, true)
            succeeds_with_value("cookbook_versions", {"cookbook" => "~> 1.0.0"}, nil, true)
            succeeds_with_value("cookbook_versions", {"cookbook" => "1.0.0"}, nil, true)

            fails_with_value("cookbook_versions", {"cookbook" => nil},
                             "Invalid value 'null' for cookbook_versions", true)
            fails_with_value("cookbook_versions", {"cookbook" => [">= 1.0.0"]},
                             "Invalid value '[huh]' for cookbook_versions", true)
            fails_with_value("cookbook_versions", {"cookbook" => []},
                             "Invalid value '[huh]' for cookbook_versions", true)
            fails_with_value("cookbook_versions", {"cookbook" => ">= 1.0.0.0"},
                             "Invalid value '>= 1.0.0.0' for cookbook_versions", true)
            fails_with_value("cookbook_versions", {"cookbook" => ">= 1,0,0"},
                             "Invalid value '>= 1,0,0' for cookbook_versions", true)
            fails_with_value("cookbook_versions", {"cookbook" => ">= 1.a.b"},
                             "Invalid value '>= 1.a.b' for cookbook_versions", true)
            fails_with_value("cookbook_versions", {"cookbook" => ">= 1.0rc1"},
                             "Invalid value '>= 1.0rc1' for cookbook_versions", true)
            fails_with_value("cookbook_versions", {"cookbook" => ">=1.0.0"},
                             "Invalid value '>=1.0.0' for cookbook_versions", true)
            fails_with_value("cookbook_versions", {"cookbook" => " >= 1.0.0"},
                             "Invalid value ' >= 1.0.0' for cookbook_versions", true)
            fails_with_value("cookbook_versions", {"cookbook" => ">=  1.0.0"},
                             "Invalid value '>=  1.0.0' for cookbook_versions", true)
            fails_with_value("cookbook_versions", {"cookbook" => [">= 1.0", ">= 2.0"]},
                             'Invalid value \'[<<">= 1.0">>,<<">= 2.0">>]\'' +
                             ' for cookbook_versions', true)
            fails_with_value("cookbook_versions", {"cookbook" => 1},
                             "Invalid value '1' for cookbook_versions", true)
            fails_with_value("cookbook_versions", {"cookbook" => 1.1},
                             "Invalid value '1.1' for cookbook_versions", true)
            fails_with_value("cookbook_versions", {"cookbook" => ""},
                             "Invalid value '' for cookbook_versions", true)

            # test version integer constraints
            # datestamp version
            succeeds_with_value("cookbook_versions", {"cookbook" => ">= 1.2.20130730201745"}, nil, true)

            # negative version
            fails_with_value("cookbook_versions", {"cookbook" => ">= 1.-2.3"},
                             "Invalid value '>= 1.-2.3' for cookbook_versions", true)

            # 4-byte version
            succeeds_with_value("cookbook_versions", {"cookbook" => ">= 1.2.2147483647"}, nil, true)

            # 4-byte overflow version
            succeeds_with_value("cookbook_versions", {"cookbook" => ">= 1.2.2147483669"}, nil, true)

            # 8-byte overflow version
            fails_with_value("cookbook_versions", {"cookbook" => ">= 1.2.9223372036854775849"},
                             "Invalid value '>= 1.2.9223372036854775849' for cookbook_versions", true)
          end
        end
      end

      # Test patchiness
      def self.succeeds_changing_value(name, value)
        context "when updating #{name} = #{value.inspect}" do
          let(:request_payload)    { updated_environment }
          let(:expected_response)  { ok_exact_response }
          let(:success_message)    { updated_environment }

          let(:updated_environment) { full_environment(new_environment_name).merge({ name => value }) }

          should_respond_with 200, 'and update the environment' do
            # Make sure updates have persisted
            get(request_url, requestor).should look_like expected_response
          end
        end
      end

      include_context "environment_body_util"

      context "when updating a single value" do
        succeeds_changing_value("description", "whooooah")
        succeeds_changing_value("cookbook_versions", { "fork" => "= 2.2" })
        succeeds_changing_value("json_class", "Chef::Environment")
        succeeds_changing_value("chef_type", "environment")
        succeeds_changing_value("default_attributes", { "arr" => "yarr" })
        succeeds_changing_value("override_attributes", { "frick" => "frack" })
      end

      context 'when updating name' do
        let(:request_payload)     { updated_environment }
        let(:expected_response)   { created_response }
        let(:success_message)     { updated_environment }

        let(:updated_environment) { full_environment(updated_name) }
        let(:updated_name)        { 'different_name' }

        after(:each) { delete(api_url("/environments/#{updated_name}"), admin_user) }

        should_respond_with 201, 'and the updated environment'
        should_respond_with 201, 'and renames the environment' do
          get(api_url("/environments/#{new_environment_name}"), admin_user).should look_like resource_not_found_response
          get(api_url("/environments/different_name"), admin_user).should look_like ok_exact_response
        end
      end

      context 'when environment already exists' do
        let(:request_payload) { full_environment(new_environment_name).with('name', existing_environment_name) }
        let(:expected_response) { conflict_exact_response }
        let(:conflict_error_message) { [ 'Environment already exists' ] }

        let(:existing_environment_name) { "existing_environment" }
        let(:existing_environment) { full_environment(existing_environment_name).with(:description, "Existing Environment") }

        let(:assume_existing_environment!) { post(api_url("/environments"), admin_user, payload: existing_environment) }
        let(:create_existing_environment_response) do
          created_response.with(:body_exact, 'uri' => api_url("/environments/#{existing_environment_name}"))
        end

        before(:each) do
          # Create and verify existing environment
          assume_existing_environment!.should look_like create_existing_environment_response
          existing_environment_should_be_untouched
        end

        after(:each) { delete_environment(admin_user, existing_environment_name) }

        # Do not memoize in a let()
        def existing_environment_should_be_untouched
          get(api_url("/environments/#{existing_environment_name}"), admin_user).
            should look_like ok_response.with(body_exact: existing_environment)
        end

        def updated_environment_should_be_untouched
          get(api_url("/environments/#{new_environment_name}"), admin_user).
            should look_like ok_response.with(body_exact: full_environment(new_environment_name))
        end

        should_respond_with 409, 'and does not rename environment' do
          existing_environment_should_be_untouched
          updated_environment_should_be_untouched
        end
      end

      context 'when renaming environment to "_default"' do
        let(:expected_response) { conflict_exact_response }
        let(:request_payload) { default_environment.with('description', 'The NEW default Chef environment') }

        let(:conflict_error_message) { [ 'Environment already exists' ] }
        let(:verify_original_response) { ok_response.with(:body_exact, full_environment(new_environment_name)) }

        let(:default_environment) do
          empty_payload.
            with('name','_default').
            with('description', 'The default Chef environment')
        end

        before(:each) do
          # Verify that _default environment is sane
          get(api_url("/environments/_default"), admin_user).should look_like ok_response.with(:body_exact, default_environment)
        end

        after(:each) do
          # Verify _default did not change
          get(api_url("/environments/_default"), admin_user).should look_like ok_response.with(:body_exact, default_environment)

          # Verify that the resource did not change
          get(api_url("/environments/#{new_environment_name}"), admin_user).should look_like verify_original_response
        end


        should_respond_with 409
      end

      # TODO: Use OSC permissions tests
      # TODO figure out if this refers to something, and implement it instead.
      # No value to skip, so:
      # skip 'permissions', :platform => :open_source
    end
  end
end
