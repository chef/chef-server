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

  # include roles for testing w/ environments roles method
  #include Pedant::RSpec::RoleUtil

  let(:requestor) { admin_user }

  # Deal with subtly different error messages/codes in one place

  let(:new_environment_name) { 'pedant_testing_environment' }
  let(:non_existent_environment_name) { 'pedant_dummy_environment' }

  context 'POST /environments' do
    let(:request_method)  { :POST }
    let(:request_url)     { api_url '/environments' }
    let(:request_payload) { full_environment(new_environment_name) }

    let(:expected_response) { resource_created_exact_response }
    let(:created_resource)  { { "uri" => api_url("/environments/#{new_environment_name}") } }
    let(:persisted_resource) { get_environment requestor, new_environment_name }

    respects_maximum_payload_size

    context 'with no additional environments' do
      before(:each) { delete_environment(admin_user, new_environment_name) }
      after(:each) { delete_environment(admin_user, new_environment_name) }


      def self.should_create_an_environment(&additional_examples)
        should_respond_with 201, 'and a correct path'

        it 'should persist the environment' do
          response.should have_status_code 201

          # Verify that it comes back properly
          persisted_resource.should look_like http_200_response.with(body_exact: full_environment(new_environment_name))
        end

        instance_eval(&additional_examples) if additional_examples
      end

      context 'when creating a valid environment', :smoke do
        before(:each) { get_environment(admin_user, new_environment_name).should have_status_code 404 }
        let(:request_payload) { new_environment(new_environment_name) }

        should_create_an_environment
      end # when creating a valid environment


      context 'when handling the payload', :pedantic do
        context 'with authentication headers' do
          # Unconverted Auth Header DSL
          let(:method) { request_method }
          let(:url)    { request_url }
          let(:body)   { request_payload }
          let(:response_should_be_successful) do
            response.
              should look_like({
              :status => 201,
              :body_exact => {
              "uri" => api_url("/environments/#{new_environment_name}")
            }
            })
          end

          let(:success_user) { admin_user }
          let(:failure_user) { outside_user }

          include_context 'handles authentication headers correctly'
        end # with authentication headers

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

      context 'when creating another "_default" environment' do
        let(:request_payload) { new_environment '_default' }
        let(:expected_response) { conflict_exact_response }
        let(:conflict_error_message) { [ "Environment already exists" ] }

        should_respond_with 409
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

          fails_with_value("blah", "anyoldvalue", "Invalid key blah in request body")
        end

        context 'for name' do
          fails_with_value("name", :delete, "Field 'name' missing", nil, false)
          fails_with_value("name", "", "Field 'name' invalid", nil, false)
          fails_with_value("name", "abc!123", "Field 'name' invalid", nil, false)
          fails_with_value("name", "abc 123", "Field 'name' invalid", nil, false)

          # TODO: Looks like we're losing the null character with the
          # ejson:decode call which is eating it silently
          # OSC is not using ejson anymore.
          # Test works on OSC, but unknown if it works on the other platforms
          context 'ejson:decode eats nulls', :skip do
            fails_with_value("name", "abc\u0000123",
                             "Field 'name' invalid")
          end

          fails_with_value("name", "大爆発",
                           "Field 'name' invalid", nil, false)
          fails_with_value("name", 1999, incorrect_json_type_name_msg, nil, false)
          fails_with_value("name", nil, incorrect_json_type_name_msg)
          fails_with_value("name", true, incorrect_json_type_name_msg, nil, false)
          fails_with_value("name", [], incorrect_json_type_name_msg, nil, false)
          fails_with_value("name", {}, incorrect_json_type_name_msg, nil, false)
        end

        context "with weird environment name" do
          let(:new_environment_name) { "ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz-0123456789" }
          let(:request_payload) { default_payload }
          let(:expected_response) { created_response }
          let(:created_resource) { { "uri" => api_url("/environments/#{new_environment_name}") } }

          should_respond_with 201, 'and the environment url'
        end

        context "for description" do
          succeeds_with_value("description", :delete)
          succeeds_with_value("description", "")
          succeeds_with_value("description", "normal text")
          succeeds_with_value("description", "これは日本語だ")
          succeeds_with_value("description",
                              "\u0048\u0065\u006c\u006c\u006f\u0020\u65e5\u672c!")

          fails_with_value("description", 1999, "Field 'description' invalid", nil, false)
        end

        context "for json_class" do
          succeeds_with_value("json_class", "Chef::Environment")
          succeeds_with_value("json_class", :delete)
          fails_with_value("json_class", "", incorrect_json_type_class_msg)
          fails_with_value("json_class", 1999, incorrect_json_type_class_msg, nil, false)
          fails_with_value("json_class", "notaclass", incorrect_json_type_class_msg, nil, false)
        end

        context "for chef_type" do
          succeeds_with_value("chef_type", "environment")
          succeeds_with_value("chef_type", :delete)

          fails_with_value("chef_type", "", incorrect_json_type_type_msg)
          fails_with_value("chef_type", 1999, incorrect_json_type_type_msg)
          fails_with_value("chef_type", "notaclass", incorrect_json_type_type_msg)
        end

        context "for default_attributes" do
          succeeds_with_value("default_attributes", :delete)
          succeeds_with_value("default_attributes", {})
          succeeds_with_value("default_attributes", {"key1" => "value1",
                              "key2" => "value2"})

          fails_with_value("default_attributes", 1999, "Field 'default_attributes' is not a hash", nil, false)
          fails_with_value("default_attributes", "hello",
                           "Field 'default_attributes' is not a hash", nil, false)

          context "for keys" do
            succeeds_with_value("default_attributes", {"key" => "value"})
            succeeds_with_value("default_attributes", {"鍵" => "value"})

            succeeds_with_value("default_attributes", {"" => "value"})

            fails_with_value("default_attributes", '{null:"value"}',
                             "Field 'default_attributes' is not a hash", nil, false)
            fails_with_value("default_attributes", '{99:"value"}',
                              "Field 'default_attributes' is not a hash", nil, false)
            fails_with_value("default_attributes", '{"key":"value","key":"value"}',
                             "Field 'default_attributes' is not a hash", nil, false)
          end

          context "for values" do
            succeeds_with_value("default_attributes", {"key" => ""})
            succeeds_with_value("default_attributes", {"key" => nil})
            succeeds_with_value("default_attributes", {"key" => 99})
            succeeds_with_value("default_attributes", {"key" => "値"})
          end
        end

        context "for override_attributes" do
          succeeds_with_value("override_attributes", :delete)
          succeeds_with_value("override_attributes", {})
          succeeds_with_value("override_attributes", {"key1" => "value1",
                              "key2" => "value2"})

          # These cause internal server errors in Ruby
          fails_with_value("override_attributes", 1999,
                           "Field 'override_attributes' is not a hash", nil, false)
          fails_with_value("override_attributes", "hello",
                           "Field 'override_attributes' is not a hash", nil, false)
          context "for keys" do
            succeeds_with_value("override_attributes", {"key" => "value"})
            succeeds_with_value("override_attributes", {"鍵" => "value"})

            succeeds_with_value("override_attributes", {"" => "value"})

            # These cause internal server errors in Ruby
            fails_with_value("override_attributes", '{null:"value"}',
                             "Field 'override_attributes' is not a hash", nil, false)
            fails_with_value("override_attributes", '{99:"value"}',
                             "Field 'override_attributes' is not a hash", nil, false)
            fails_with_value("override_attributes", '{"key":"value","key":"value"}',
                             "Field 'override_attributes' is not a hash", nil, false)
          end

          context "for values" do
            succeeds_with_value("override_attributes", {"key" => ""})
            succeeds_with_value("override_attributes", {"key" => nil})
            succeeds_with_value("override_attributes", {"key" => 99})
            succeeds_with_value("override_attributes", {"key" => "値"})
          end
        end

        context "for cookbooks" do
          succeeds_with_value("cookbook_versions", :delete)
          succeeds_with_value("cookbook_versions", {})

          # These cause internal server errors in Ruby
          fails_with_value("cookbook_versions", 1999,
                           "Field 'cookbook_versions' is not a hash", nil, false)
          fails_with_value("cookbook_versions", "hello",
                           "Field 'cookbook_versions' is not a hash", nil, false)

          context "for cookbook names" do
            succeeds_with_value("cookbook_versions", {"cookbook" => ">= 1.0.0"})

            fails_with_value("cookbook_versions", {"the cookbook" => ">= 1.0.0"},
                             "Invalid key 'the cookbook' for cookbook_versions", nil, false)
            fails_with_value("cookbook_versions", {"料理書" => ">= 1.0.0"},
                             "Invalid key '料理書' for cookbook_versions", nil, false)
            fails_with_value("cookbook_versions", {"" => ">= 1.0.0"},
                             "Invalid key '' for cookbook_versions", nil, false)
            fails_with_value("cookbook_versions", '{null:">= 1.0.0"}',
                             "Field 'cookbook_versions' is not a hash", nil, false)
            fails_with_value("cookbook_versions", '{1999:">= 1.0.0"}',
                             "Field 'cookbook_versions' is not a hash", nil, false)
          end

          context "for versions" do
            succeeds_with_value("cookbook_versions", {"cookbook" => ">= 1.0.0"})
            succeeds_with_value("cookbook_versions", {"cookbook" => ">= 1.0"})
            succeeds_with_value("cookbook_versions", {"cookbook" => "<= 1.0.0"})
            succeeds_with_value("cookbook_versions", {"cookbook" => "> 1.0.0"})
            succeeds_with_value("cookbook_versions", {"cookbook" => "< 1.0.0"})
            succeeds_with_value("cookbook_versions", {"cookbook" => "= 1.0.0"})
            succeeds_with_value("cookbook_versions", {"cookbook" => "~> 1.0.0"})
            succeeds_with_value("cookbook_versions", {"cookbook" => "1.0.0"})

            fails_with_value("cookbook_versions", {"cookbook" => nil},
                             "Invalid value 'null' for cookbook_versions")
            fails_with_value("cookbook_versions", {"cookbook" => [">= 1.0.0"]},
                             "Invalid value '[huh]' for cookbook_versions")
            fails_with_value("cookbook_versions", {"cookbook" => []},
                             "Invalid value '[huh]' for cookbook_versions")
            fails_with_value("cookbook_versions", {"cookbook" => ">= 1.0.0.0"},
                             "Invalid value '>= 1.0.0.0' for cookbook_versions")
            fails_with_value("cookbook_versions", {"cookbook" => ">= 1,0,0"},
                             "Invalid value '>= 1,0,0' for cookbook_versions")
            fails_with_value("cookbook_versions", {"cookbook" => ">= 1.a.b"},
                             "Invalid value '>= 1.a.b' for cookbook_versions")
            fails_with_value("cookbook_versions", {"cookbook" => ">= 1.0rc1"},
                             "Invalid value '>= 1.0rc1' for cookbook_versions")
            fails_with_value("cookbook_versions", {"cookbook" => ">=1.0.0"},
                             "Invalid value '>=1.0.0' for cookbook_versions")
            fails_with_value("cookbook_versions", {"cookbook" => " >= 1.0.0"},
                             "Invalid value ' >= 1.0.0' for cookbook_versions")
            fails_with_value("cookbook_versions", {"cookbook" => ">=  1.0.0"},
                             "Invalid value '>=  1.0.0' for cookbook_versions")
            fails_with_value("cookbook_versions", {"cookbook" => [">= 1.0", ">= 2.0"]},
                             'Invalid value \'[<<">= 1.0">>,<<">= 2.0">>]\'' +
                             ' for cookbook_versions')
            fails_with_value("cookbook_versions", {"cookbook" => 1},
                             "Invalid value '1' for cookbook_versions")
            fails_with_value("cookbook_versions", {"cookbook" => 1.1},
                             "Invalid value '1.1' for cookbook_versions")
            fails_with_value("cookbook_versions", {"cookbook" => ""},
                             "Invalid value '' for cookbook_versions")

            # test version integer constraints
            # datestamp version
            succeeds_with_value("cookbook_versions", {"cookbook" => ">= 1.2.20130730201745"})

            # negative version
            fails_with_value("cookbook_versions", {"cookbook" => ">= 1.-2.3"},
                             "Invalid value '>= 1.-2.3' for cookbook_versions")

            # 4-byte version
            succeeds_with_value("cookbook_versions", {"cookbook" => ">= 1.2.2147483647"})

            # 4-byte overflow version
            succeeds_with_value("cookbook_versions", {"cookbook" => ">= 1.2.2147483669"})

            # 8-byte overflow version
            fails_with_value("cookbook_versions", {"cookbook" => ">= 1.2.9223372036854775849"},
                             "Invalid value '>= 1.2.9223372036854775849' for cookbook_versions")

          end
        end
      end # when validating

      context 'when creating an environment without json_class' do
        before(:each) { get_environment(admin_user, new_environment_name).should have_status_code 404 }

        let(:request_payload) { new_environment(new_environment_name).except('json_class') }

        should_create_an_environment
      end # when creating an environment without json_class

    end # with no additional environments

    context 'with non-default environments' do

      before(:each) { add_environment(admin_user, full_environment(new_environment_name)) }
      after(:each)  { delete_environment(admin_user, new_environment_name) }

      context 'with the same name as another non-default environment' do
        let(:request_payload) { new_environment(new_environment_name) }
        let(:expected_response) { conflict_exact_response }
        let(:conflict_error_message) { ["Environment already exists"] }

        should_respond_with 409
      end

    end # with non-default environments
  end # POST /environments
end # Environments Endpoint
