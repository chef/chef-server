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

require 'pedant/rspec/role_util'
require 'pedant/rspec/environment_util'
require 'pedant/rspec/validations'

describe "Testing the Roles API endpoint", :roles do
  include Pedant::RSpec::RoleUtil
  include Pedant::RSpec::EnvironmentUtil

  let(:admin_requestor){admin_user}
  let(:requestor){admin_requestor}

  let(:nonexistent_role_name){unique_name('nonexistent_pedant_role')}

  context 'making a request to /roles' do
    let(:request_url){api_url("/roles")}

    context 'using GET' do
      let(:request_method){:GET}

      context 'with no roles' do
        let(:roles){ {} }
        it 'returns a 200 and an empty hash' do
          should look_like fetch_roles_list_response
        end
      end

      context 'with roles' do
        include_context 'with temporary testing role'
        let(:roles){ {role_name => api_url("/roles/#{role_name}")}}
        it 'returns a 200 and a hash of name -> url' do
          should look_like fetch_roles_list_response
        end
      end
    end # GET

    context 'using POST' do
      include Pedant::RSpec::Validations::Create
      let(:request_method){:POST}

      let(:role_name){unique_name('pedant_testing_role')}
      let(:role){new_role(role_name)}
      let(:request_payload){role}
      let(:fetched_resource_response){ get(api_url("/roles/#{role_name}"), requestor) }

      after :each do
        delete_role(admin_requestor, role_name)
      end

      context 'for a role that does not exist', :smoke do
        should_successfully_create_a_role
      end

      context 'when validating' do
        let(:resource_url){api_url("/roles/#{resource_name}")}
        let(:default_resource_attributes){new_role(role_name)}
        let(:persisted_resource_response){ get(resource_url, requestor) }

        after(:each){ delete_role(admin_requestor, resource_name)}

        context "the 'name' field" do
          let(:validate_attribute) {'name'}

          accepts_valid_value 'pedant_role'
          accepts_valid_value 'PEDANT_ROLE'
          accepts_valid_value 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqurstuvwxyz0123456789-_:'

          # Need to be able to not try and delete these
          rejects_invalid_value "this+ is bad!!!"
          rejects_invalid_value "I-do-not-like!!!"
        end

        context "the 'json_class' field" do
          let(:validate_attribute){"json_class"}
          accepts_valid_value "Chef::Role"
          rejects_invalid_value "Chef::NotReallyAClass"
        end

        context "the 'chef_type' field" do
          let(:validate_attribute){"chef_type"}
          accepts_valid_value "role"
          rejects_invalid_value "node"
        end

        rejects_invalid_keys

      end

      context 'with various valid inputs' do

        should_set_default_value_for 'role', 'default_attributes', {}
        should_set_default_value_for 'role', 'override_attributes', {}
        should_set_default_value_for 'role', 'run_list', []
        should_set_default_value_for 'role', 'env_run_lists', {}

        # This just tests 'run_list', not 'env_run_lists'
        test_run_list_corner_cases :role

        # ... THIS tests 'env_run_lists' :)
        #
        # With a bit of work, this could probably be incorporated into
        # the 'test_run_list_corner_cases' helper method in common.rb,
        # but I'm not sure that'd be all that much of a win.
        def self.environment_run_list_test(context_message, example_message, run_lists)
          context context_message do
            let(:role){new_role(role_name, :env_run_lists => run_lists)}
            should_successfully_create_a_role example_message
          end
        end

        environment_run_list_test("with non-normalized environment run lists",
                                  "with normalized environment run lists",
                                  {"prod" =>["foo", "foo::bar", "bar::baz@1.0.0", "recipe[web]", "role[prod]"],
                                    "dev" => ["oof", "oof::rab", "rab::zab@0.0.1", "recipe[bew]", "role[dev]"]})

        environment_run_list_test("with environment run lists that have duplicates",
                                  "with duplicates removed",
                                  {"prod" =>["foo", "recipe[foo]", "role[prod]", "role[prod]"],
                                    "dev" => ["bar", "recipe[bar]", "role[dev]", "role[dev]"]})

        environment_run_list_test("with environment run lists that have implicit and explicit 'default' recipes",
                                  "with both versions remaining in the lists",
                                  {"prod" =>["foo", "foo::default", "recipe[foo]", "recipe[foo::default]"],
                                    "dev" => ["bar", "bar::default", "recipe[bar]", "recipe[bar::default]"]})

        environment_run_list_test("with environment run lists that have recipes named 'recipe' and 'role'",
                                  "with all oddly-named recipes intact in the run lists",
                                  {"prod"=>   ["recipe", "recipe::foo", "recipe::bar@1.0.0", "role", "role::foo", "role::bar@1.0.0",
                                    "recipe[recipe]", "recipe[role]", "role[recipe]", "role[role]"],
                                    "dev" =>  ["recipe", "recipe::foo", "recipe::bar@1.0.0", "role", "role::foo", "role::bar@1.0.0",
                                      "recipe[recipe]", "recipe[role]", "role[recipe]", "role[role]"] })

      end # various valid inputs

      context 'with various invalid inputs' do

        should_not_allow_creation_with_incorrect_types 'role', 'default_attributes', 'hash', "This is clearly not a hash"
        should_not_allow_creation_with_incorrect_types 'role', 'override_attributes', 'hash', "This is clearly not a hash"

        # This is a wee bit abusive of the testing function, since 'run_list' really isn't a type, per se
        should_not_allow_creation_with_incorrect_types 'role', 'run_list', 'valid run list', [123]

        should_not_allow_creation_with_incorrect_types 'role', 'run_list', 'valid run list', ["recipe["]

        context 'with invalid env_run_lists hashes', :validation do
          let(:expected_response) { invalid_role_response }
          let(:invalid_role_error_message) { ["Field 'env_run_lists' contains invalid run lists"] }

          context "with a non-hash value for 'env_run_lists'" do
            it 'should respnd with an error' do
              request_payload['env_run_lists'] = "This is clearly wrong"
              response.should look_like expected_response
            end
          end

          context "without proper run lists in the 'env_run_lists hash'", :validation do
            it 'should respond with an error' do
              request_payload['env_run_lists'] = {
                "preprod" => ["recipe[blah]"],
                "prod" => [123]  # This is not really a valid run list item
              }
              response.should look_like expected_response
            end
          end
        end

        respects_maximum_payload_size
      end

      context 'for a role that already exists' do
        include_context 'with temporary testing role'
        let(:request_payload){role}
        it 'fails' do
          should look_like create_role_conflict_response
        end
      end

    end # POST
  end # /roles

  context 'making a request to /roles/<role>' do
    let(:request_url){api_url("/roles/#{role_name}")}

    context 'using GET' do
      let(:request_method){:GET}

      context 'to a nonexistent role' do
        let(:role_name){nonexistent_role_name}
        it 'fails with a 404' do
          should look_like role_not_found_response
        end
      end

      context 'to a role that exists' do
        include_context 'with temporary testing role'
        it 'succeeds', :smoke do
          should look_like fetch_role_success_response
        end
      end

    end # GET

    context 'using PUT' do
      let(:request_method){:PUT}

      context 'to a nonexistent role' do
        let(:role_name){nonexistent_role_name}
        let(:request_payload){ {} }
        it 'should respond with a 404' do
          should look_like role_not_found_response
        end
      end

      context 'to a role that exists' do
        include_context 'with temporary testing role'

        let(:updated_fields){fail "supply 'updated_fields' in the form of a hash.  They will be merged with the existing role body to form a PUT payload"}
        # updated_role needed for update_role_success_response
        let(:updated_role){ update_object(role, updated_fields) }
        let(:request_payload){ updated_role }

        context 'with canonical payload', :smoke do
          let(:updated_fields){ {'description' => 'updated description'}}
          should_successfully_update_a_role
        end

        context 'with role name changed in the payload', :validation do
          let(:updated_fields){ {'name' => 'this_is_not_the_same_name_as_before'} }
          should_fail_to_update_a_role 400, 'Role name mismatch.'
        end

        context 'without role name in payload' do
          let(:updated_fields) do
            {
              'name' => :DELETE, # this will remove the name from the payload
              'description' => 'I was updated based on the name in the URL'
            }
          end
          should_successfully_update_a_role
        end

        context 'with no name, but other invalid information in payload', :validation do
          let(:updated_fields) do
            {
              'name' => :DELETE,  # this will remove the name from the payload
              'description' => 'No good will come of this',
              'json_class' => 'Chef::Node' # <- This is invalid for Roles!  This should prevent the update.
            }
          end

          should_fail_to_update_a_role 400, "Field 'json_class' invalid"
        end

        respects_maximum_payload_size

      end
    end # PUT

    context 'using DELETE' do
      let(:request_method){:DELETE}

      context 'to a nonexistent role' do
        let(:role_name){nonexistent_role_name}
        it 'fails with a 404' do
          should look_like role_not_found_response
        end
      end

      context 'to an existing role', :smoke do
        include_context 'with temporary testing role'
        should_successfully_delete_a_role
      end

    end # DELETE
  end # /roles/<role>

  context 'making a request to /roles/<role>/environments' do
    let(:request_url){api_url("/roles/#{role_name}/environments")}
    include_context 'with temporary testing environment'

    let(:role_environment_names){fail 'Please specify which environments the role should be in'}
    let(:fetch_role_environment_success_response) do
      {
        :status => 200,
        :body => role_environment_names
      }
    end

    context 'GET' do
      let(:request_method){:GET}

      context 'for a nonexistent role' do
        let(:role_name){"this_role_does_not_exist"}
        it 'fails' do
          should look_like role_not_found_response
        end
      end

      context 'for a role with no environment run lists' do
        include_context 'with temporary testing role'
        let(:role_environment_names){["_default"]}
        it 'should return just the default environment' do
          should look_like fetch_role_environment_success_response
        end
      end

      context 'for a role with one additional environment run list' do
        include_context 'with temporary testing role' do
          let(:role_env_run_lists){ {environment_name => ['zippy']} }
        end
        let(:role_environment_names){["_default", environment_name]}

        it 'should return the default environment and the additional environment' do
          should look_like fetch_role_environment_success_response
        end
      end

      context 'for a role with multiple additional environment run lists' do
        let(:nonexistent_environment_name){'this_environment_does_not_really_exist'}

        include_context 'with temporary testing role' do
          let(:role_env_run_lists) do
            {
              environment_name => ['zippy'],
              nonexistent_environment_name => []
            }
          end
        end

        let(:role_environment_names){["_default", environment_name, nonexistent_environment_name]}
        it 'returns multiple environments, even if some do not exist' do
          # Ensure that the environment really doesn't exist
          get(api_url("/environments/#{nonexistent_environment_name}"), requestor).should look_like resource_not_found_response

          # Then perform the request we're actually testing
          should look_like fetch_role_environment_success_response
        end
      end

    end
  end # /roles/<role>/environments

  context 'making a request to /roles/<role>/environments/<environment>', :environments do
    let(:request_url){api_url("/roles/#{role_name}/environments/#{environment_name}")}

    context 'using GET' do
      let(:request_method){:GET}

      context 'with the default environment' do
        let(:environment_name){"_default"}

        context 'to a nonexistent role' do
          let(:role_name){nonexistent_role_name}
          it 'responds with a 404 for the role' do
            should look_like role_not_found_response
          end
        end

        context 'with an already existing role' do
          include_context 'with temporary testing role' do
            let(:role_run_list){["recipe[nginx]"]}
          end
          it "responds with 200 and the role's run list", :smoke do
            should look_like({
                               :status => 200,
                               :body_exact => {
                                 "run_list" => role_run_list
                               }
                             })
          end
        end
      end # default environment

      context 'with a non-default environment' do
        include_context 'with temporary testing environment'

        context 'for a role with a run list in that environment' do
          include_context 'with temporary testing role' do
            let(:run_list_for_environment){["recipe[nginx]"]}
            let(:role_env_run_lists){{environment_name => run_list_for_environment}}
          end
          it "responds with 200 and the role's run list" do
            should look_like({
                               :status => 200,
                               :body_exact => {
                                 "run_list" => run_list_for_environment
                               }
                             })
          end
        end # role in envinronment

        context 'for a role with no run list in that environment' do
          # NB: This setup only works because we can refer to non-existent
          # environments
          let(:other_environment_name){"some_other_environment"}
          include_context 'with temporary testing role' do
            let(:run_list_for_environment){["recipe[nginx]"]}
            let(:role_env_run_lists){{other_environment_name => run_list_for_environment}}
          end
          it "responds with 200 and a null run list" do
            should look_like({
                               :status => 200,
                               :body_exact => {
                                 "run_list" => nil
                               }
                             })
          end
        end # role not in environment
      end # non-default environment

      context 'with a non-existent environment' do
        let(:environment_name){"this_environment_does_not_exist"}
        include_context 'with temporary testing role' do
          let(:run_list_for_environment){["recipe[nginx]"]}
          # Currently, no active validation is in place to prevent
          # input such as this (referring to an environment that does
          # not exist)
          let(:role_env_run_lists){{environment_name => run_list_for_environment}}
          let(:environment_not_found_message){environment_not_found_message_alternate}
        end
        it 'responds with a 404 for the environment' do
          should look_like environment_not_found_response
        end
      end
    end # GET
  end # /roles/<role>/environments/<environment>

end # describe Roles endpoint
