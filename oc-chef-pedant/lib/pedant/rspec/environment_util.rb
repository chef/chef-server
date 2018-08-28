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
require 'pedant/rspec/matchers'
require 'rspec/core/shared_context'

module Pedant
  module RSpec
    module EnvironmentUtil
      extend ::Pedant::Concern
      extend ::RSpec::Core::SharedContext

      # Error messages
      let(:failed_to_authenticate_as_invalid_msg) { ["Failed to authenticate as '#{invalid_user.name}'. Ensure that your node_name and client key are correct."] }
      let(:cannot_load_nonexistent_env_msg) { ["Cannot load environment #{non_existent_environment_name}"] }
      let(:cannot_load_new_env_msg) { ["Cannot load environment #{new_environment_name}"] }
      let(:incorrect_json_type_body_msg) { self.class.incorrect_json_type_body_msg }

      module ClassMethods

        def incorrect_json_type_body_msg
          'Incorrect JSON type for request body'
        end

        def must_supply_data_msg
          'Must supply environment data'
        end

        def incorrect_json_type_name_msg
          "Field 'name' invalid"
        end

        def incorrect_json_type_class_msg
          "Field 'json_class' invalid"
        end

        def incorrect_json_type_type_msg
          "Field 'chef_type' invalid"
        end
      end

      let(:environment_not_found_message){"environment '#{environment_name}' not found"}

      # TODO: This is a holdover from Ruby days that snuck into Erchef.  Consolidate!
      let(:environment_not_found_message_alternate){"Cannot load environment #{environment_name}"}

      let(:environment_not_found_response) do
        {
          :status => 404,
          :body_exact => {
            "error" => [environment_not_found_message]
          }
        }
      end

      # When you include this context, 'environment_name' is set to the
      # name of the testing environment
      shared_context 'with temporary testing environment' do
        let(:environment_name){unique_name("temporary_environment")}
        let(:environment_description){"A Pedant Environment"}
        let(:environment_cookbook_versions){ {} }
        let(:environment_cookbook_versions){ {} }
        let(:environment_default_attributes){ {} }
        let(:environment_override_attributes){ {} }
        let(:environment_payload) do
          {
            'name' => environment_name,
            'json_class' => 'Chef::Environment',
            'chef_type' => 'environment',
            'description' => environment_description,
            'cookbook_versions' => environment_cookbook_versions,
            'default_attributes' => environment_default_attributes,
            'override_attributes' => environment_override_attributes
          }
        end
        let(:environment) { environment_payload }

        before :each do
          add_environment(admin_requestor, environment_payload)
        end

        after :each do
          delete_environment(admin_requestor, environment_name)
        end
      end # shared context


      def new_environment(name)
        {
          "name" => name,
          "json_class" => "Chef::Environment", # Absolutely required due to Ruby JSON magic
          'chef_type' => 'environment',
          'description' => '',
          'cookbook_versions' => {},
          'default_attributes' => {},
          'override_attributes' => {}
        }
      end

      def full_environment(name, attributes = {})
        {
          "name" => name,
          "description" => "Behold, a testing environment!",
          "cookbook_versions" => {
            "ultimatecookbook" => "= 1.1.0"
          },
          "json_class" => "Chef::Environment",
          "chef_type" => "environment",
          "default_attributes" => {
            "defaultattr" => "y"
          },
          "override_attributes" => {
            "overrideattr" => "b"
          }
        }.merge(attributes)
      end

      def default_environment
        {
          "name" => "_default",
          "description" => "The default Chef environment",
          "cookbook_versions" => {},
          "json_class" => "Chef::Environment",
          "chef_type" => "environment",
          "default_attributes" => {},
          "override_attributes" => {}
        }
      end

      def add_environment(requestor, environment)
        post(api_url("/environments"), requestor, :payload => environment)
      end

      def delete_environment(requestor, environment_name)
        delete(api_url("/environments/#{environment_name}"), requestor)
      end

      # The functionality of this is tested already; this is just here for
      # making subsequent tests less verbose
      def get_environment(requestor, environment_name)
        get(api_url("/environments/#{environment_name}"), requestor)
      end
    end

    shared_context "environment_body_util" do
      let(:default_payload) {
        {
          'name' => new_environment_name,
          'chef_type' => 'environment',
          'json_class' => 'Chef::Environment',
          'description' => 'A florid description',
          'cookbook_versions' => {
            'schmapache' => '= 1.5.1'
          },
          'default_attributes' => {
            'a' => 'b'
          },
          'override_attributes' => {
            'a' => 'b'
          }
        }
      }
      let(:empty_payload) {
        {
          'name' => '',
          'chef_type' => 'environment',
          'json_class' => 'Chef::Environment',
          'description' => '',
          'cookbook_versions' => {},
          'default_attributes' => {},
          'override_attributes' => {}
        }
      }

      def make_payload(overwrite_variables, defaults = nil)
        result = defaults || default_payload
        if (overwrite_variables[:skip_delete])
          overwrite_variables.delete(:skip_delete)
          skip_delete = true
        end
        overwrite_variables.each do |variable,value|
          if value == :delete
            if (skip_delete)
              result[variable] = empty_payload[variable]
            else
              result.delete(variable)
            end
          else
            result[variable] = value
          end
        end
        return result
      end

      def self.fails_with_value(variable, value, expected_error, existing_environment = nil,
                                pending = false)
        if (pending)
          it "with #{variable} = #{value} it reports 400", :validation, :skip do
          end
        else
          it "with #{variable} = #{value} it reports 400", :validation do
            if (existing_environment)
              response = put(api_url("/environments/#{new_environment_name}"), admin_user,
                             :payload => make_payload(variable => value))
            else
              response = post(api_url("/environments"), admin_user,
                              :payload => make_payload(variable => value))
            end
            response.should have_error(400, expected_error)
          end
        end
      end

      def self.succeeds_with_value(variable, value, expected_value = nil,
                                   existing_environment = nil, pending = false)
        expected_value ||= value
        if (pending)
          it "with #{variable} = #{value} it succeeds", :skip do
          end
        else
          it "with #{variable} = #{value} it succeeds" do
            if (existing_environment)
              response = put(api_url("/environments/#{new_environment_name}"), admin_user,
                             :payload => make_payload(variable => value))
              response.should look_like({
                                          :status => 200,
                                          :body_exact => make_payload(variable => value,
                                                                      :skip_delete => true)
                                        })
            else
              response = post(api_url("/environments"), admin_user,
                              :payload => make_payload(variable => value))
              response.should look_like({
                                          :status => 201,
                                          :body_exact => {
                                            "uri" => api_url("/environments/" +
                                                             "#{new_environment_name}")
                                          }})
            end

            response = get(api_url("/environments/#{new_environment_name}"), admin_user)
            response.should look_like({
                                        :status => 200,
                                        :body_exact => make_payload(variable => value,
                                                                    :skip_delete => true) })
          end
        end
      end
    end
  end
end
