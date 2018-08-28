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

require 'pedant/rspec/environment_util'
require 'pedant/rspec/cookbook_util'

describe "/environments/ENVIRONMENT/recipes API endpoint", :environments do
  include Pedant::RSpec::CookbookUtil
  include Pedant::RSpec::EnvironmentUtil

  # Cookbook tests are parameterized to support common testing of both
  # /cookbooks and /cookbook_artifacts, so we need to specify that we want to
  # talk to /cookbooks
  let(:cookbook_url_base) { "cookbooks" }

  let(:request_method)   { :GET }
  let(:request_url)      { api_url "/environments/#{environment_name}/recipes" }
  let(:requestor)        { admin_user }
  let(:environment_name) { env }

  include_context "environment_body_util"

  # TODO: Refactor
  def self.env; 'test_env'; end
  def self.default; '_default'; end

  shared(:env)     { 'test_env' }
  shared(:default) { '_default' }

  before(:all) { add_environment(admin_user, new_environment(env)) }
  after(:all)  { delete_environment(admin_user, env) }

  def self.should_respond_with_success(message)
    it ["should respond with 200 OK", message].compact.join(' ') do
      should have_status_code 200
      parsed_response.should =~ expected_recipes
    end
  end

  context 'with no cookbooks' do
    context 'when environment does not exist' do
      let(:expected_response)       { resource_not_found_exact_response }
      let(:environment_name) { non_existent_environment }
      let(:non_existent_environment) { 'bad_env' }

      let(:not_found_error_message) { ["Cannot load environment #{non_existent_environment}"] }
      should_respond_with 404
    end

    context 'when fetching recipes' do
      let(:expected_recipes) { [] }
      should_respond_with_success 'and no recipes'
    end

    context 'when fetching recipes from _default environment' do
      let(:environment_name) { default }
      let(:expected_recipes) { [] }
      should_respond_with_success 'and no recipes'
    end
  end

  context 'with multiple versions of multiple cookbooks' do
    let(:cookbooks) do
      {
        "pedant_cb_one" =>
        {
          "1.0.0" => ['webserver'],
          "2.0.0" => ['database', 'webserver'],
          "3.0.0" => ['awesome_sauce', 'database', 'webserver']
        },
          "pedant_cb_two" =>
        {
          "1.0.0" => ['chicken'],
          "1.2.0" => ['beef', 'chicken'],
          "1.2.5" => ['beef', 'chicken', 'stewed_monkey_brains']
        },
          "pedant_cb_three" =>
        {
          "0.0.1" => ['server'],
          "0.5.0" => ['client', 'server'],
          "1.0.0" => ['client', 'replication', 'server']
        }
      }
    end

    before(:each) { setup_cookbooks(cookbooks) }
    after(:each)  { remove_cookbooks(cookbooks) }

    context 'with no environment constraints' do
      let(:expected_recipes) do
        %w( pedant_cb_one::awesome_sauce
            pedant_cb_one::database
            pedant_cb_one::webserver
            pedant_cb_three::client
            pedant_cb_three::replication
            pedant_cb_three::server
            pedant_cb_two::beef
            pedant_cb_two::chicken
            pedant_cb_two::stewed_monkey_brains )
      end

      # These smoke tests may be too slow
      context 'when fetching recipes from a non-default environment', :smoke do
        should_respond_with_success 'and recipes from the latest version of all cookbooks within the environment'
      end

      context 'when fetching recipes from _default environment', :smoke do
        let(:environment_name) { default }
        should_respond_with_success 'and recipes from the latest version of all cookbooks within the environment'
      end
    end

    def self.test_with_constraints(constraint_hash, expected_recipes)
      context "with constraints #{constraint_hash.inspect}" do
        ## Temporary work-around until I can fix the environment_body_util shared context
        let(:new_environment_name) { env }

        before :each do
          # Add constraints to environment
          put(api_url("/environments/#{env}"), admin_user,
              :payload => make_payload('cookbook_versions' => constraint_hash))
        end

        after :each do
          # Remove constraints from the environment
          put(api_url("/environments/#{env}"), admin_user,
              :payload => make_payload('cookbook_versions' => {}))
        end

        it 'retrieves appropriate recipes' do
          get(api_url("/environments/#{env}/recipes"), admin_user) do |response|
            response.should have_status_code 200
            parse(response).should =~ expected_recipes
          end
        end
      end
    end

    context 'with environment constraints' do
      test_with_constraints({
                              'pedant_cb_one' => '= 1.0.0'
                            },
                            [
                             'pedant_cb_one::webserver',
                             'pedant_cb_three::client',
                             'pedant_cb_three::replication',
                             'pedant_cb_three::server',
                             'pedant_cb_two::beef',
                             'pedant_cb_two::chicken',
                             'pedant_cb_two::stewed_monkey_brains'
                            ])

      test_with_constraints({
                              'pedant_cb_one' => '< 2.5.0'
                            },
                            [
                             'pedant_cb_one::database',
                             'pedant_cb_one::webserver',
                             'pedant_cb_three::client',
                             'pedant_cb_three::replication',
                             'pedant_cb_three::server',
                             'pedant_cb_two::beef',
                             'pedant_cb_two::chicken',
                             'pedant_cb_two::stewed_monkey_brains'
                            ])

      test_with_constraints({
                              'pedant_cb_one' => '< 2.5.0',
                              'pedant_cb_two' => '= 1.0.0'
                            },
                            [
                             'pedant_cb_one::database',
                             'pedant_cb_one::webserver',
                             'pedant_cb_three::client',
                             'pedant_cb_three::replication',
                             'pedant_cb_three::server',
                             'pedant_cb_two::chicken'
                            ])

      # Nothing satisfies these constraints!
      # 6.6.6 = The Semantic Version of the Beast
      test_with_constraints({
                              'pedant_cb_one' => '= 6.6.6',
                              'pedant_cb_two' => '= 6.6.6',
                              'pedant_cb_three' => '= 6.6.6',
                            },
                            [])
    end
  end
end
