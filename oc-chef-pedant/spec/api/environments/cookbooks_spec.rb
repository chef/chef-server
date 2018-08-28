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
require 'pedant/rspec/search_util'
require 'pedant/rspec/role_util'
require 'pedant/rspec/cookbook_util'

describe "/environments/ENVIRONMENT/cookbooks API endpoint", :environments, :cookbooks do
  include Pedant::RSpec::CookbookUtil
  include Pedant::RSpec::EnvironmentUtil

  # Cookbook tests are parameterized to support common testing of both
  # /cookbooks and /cookbook_artifacts, so we need to specify that we want to
  # talk to /cookbooks

  let(:cookbook_url_base) { "cookbooks" }
  let(:request_method)   { :GET }
  let(:request_url)      { api_url "/environments/#{environment_name}/cookbooks" }
  let(:requestor)        { admin_user }
  let(:environment_name) { env }


  include_context "environment_body_util" # from EnvironmentUtil

  let(:env){ "test_env" }
  let(:default){ "_default" }

  before(:each) { add_environment(admin_user, new_environment(env)) }
  after(:each)  { delete_environment(admin_user, env) }

  context "with no cookbooks" do

    context 'when fetching cookbooks from "_default" environment' do
      let(:request_url)       { api_url "/environments/#{default}/cookbooks" }
      let(:expected_response) { ok_exact_response }
      let(:success_message)   { { } } # No cookbooks
      should_respond_with 200, 'and no cookbooks'
    end

    context 'when environment does not exist' do
      let(:expected_response) { resource_not_found_exact_response }
      let(:environment_name)  { non_existent_environment }
      let(:non_existent_environment) { 'bad_env' }
      let(:not_found_error_message) { ["Cannot load environment #{non_existent_environment}"] }

      should_respond_with 404
    end

    context 'when fetching cookbooks' do
      let(:expected_response) { ok_exact_response }
      let(:success_message)   { { } } # No cookbooks
      should_respond_with 200, 'and no cookbooks'
    end

    ['all', 0, 1, 2, 30].each do |num|
      context "when fetching cookbooks with num_version=#{num}" do
        let(:request_url)       { api_url "/environments/#{environment_name}/cookbooks?num_versions=#{num}" }
        let(:expected_response) { ok_exact_response }
        let(:success_message)   { { } } # No cookbooks
        should_respond_with 200, 'and no cookbooks'
      end
    end

    context 'when num_versions is not "all" or a number', :validation do
      let(:request_url)       { api_url "/environments/#{environment_name}/cookbooks?num_versions=skittles" }
      let(:expected_response) { bad_request_exact_response }
      let(:error_message)     { ["You have requested an invalid number of versions (x >= 0 || 'all')"] }
      should_respond_with 400
    end

  end # with no cookbooks

  context "with multiple versions of multiple cookbooks" do
    before(:each) { setup_cookbooks(cookbooks) }
    after(:each)  { remove_cookbooks(cookbooks) }

    let(:cookbooks) do
      {
        "pedant_cb_one" =>
        {
          "1.0.0" => [],
          "2.0.0" => [],
          "3.0.0" => []
        },
          "pedant_cb_two" =>
        {
          "1.0.0" => [],
          "1.2.0" => [],
          "1.2.5" => []
        },
          "pedant_cb_three" =>
        {
          "0.0.1" => [],
          "0.5.0" => [],
          "1.0.0" => []
        },
      }
    end


    # Generate the expected JSON body from a set of cookbook specs for
    # a call to the /environments/ENVIRONMENT/cookbooks endpoint
    def expected_for_cookbooks(cookbooks, num_versions)
      latest = get_latest_cookbooks(cookbooks, num_versions)
      latest.inject({}) do |body, cookbook_spec|
        name, version_specs  = cookbook_spec
        body[name] = {
          "url" => api_url("/cookbooks/#{name}"),
          "versions" => version_specs.map do |version_string, recipe_names|
            {
              "url" => api_url("/cookbooks/#{name}/#{version_string}"),
              "version" => version_string
            }
          end
        }
        body
      end
    end

    context 'with no environment constraints' do
      context 'from a non-default environment' do
        context 'when fetching cookbooks', :smoke do
          let(:expected_response) { ok_exact_response }
          let(:success_message)   { expected_for_cookbooks(cookbooks, 1) }
          should_respond_with 200, 'and latest versions of ALL cookbooks'
        end

        ['all',1,2,3,30].each do |num|
          context "when fetching cookbooks with num_version=#{num}" do
            let(:request_url)       { api_url "/environments/#{environment_name}/cookbooks?num_versions=#{num}" }
            let(:expected_response) { ok_exact_response }
            let(:success_message)   { expected_for_cookbooks(cookbooks, num) }
            should_respond_with 200, "and #{num} latest versions of cookbooks"
          end
        end
      end

      context 'from the _default environment' do
        context 'when fetching cookbooks in the _default environment' do
          let(:request_url)       { api_url "/environments/#{default}/cookbooks" }
          let(:expected_response) { ok_exact_response }
          let(:success_message)   { expected_for_cookbooks(cookbooks, 1) }
          should_respond_with 200, 'and latest versions of ALL cookbooks'
        end

        ['all',1,2,3,30].each do |num|
          context "when fetching cookbooks with num_version=#{num}" do
            let(:request_url)       { api_url "/environments/#{default}/cookbooks?num_versions=#{num}" }
            let(:expected_response) { ok_exact_response }
            let(:success_message)   { expected_for_cookbooks(cookbooks, num) }
            should_respond_with 200, "and #{num} latest versions of cookbooks"
          end
        end
      end
    end

    def self.test_with_constraints(constraint_hash, expected_pedant_cb_and_versions, num_versions=nil)
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

        # Doing this to make the tests less verbose; explicitly
        # passing in a data structure that represents the basics of
        # the response because I don't feel like recapitulating the
        # environment filtering logic for a test helper function :)
        def expected_filtered_response(spec)
          spec.inject({}) do |body, kv|
            cookbook, allowed_versions = kv
            body[cookbook] = {
              "url" => api_url("/cookbooks/#{cookbook}"),
              "versions" => allowed_versions.map do |version|
                {
                  "url" => api_url("/cookbooks/#{cookbook}/#{version}"),
                  "version" => version
                }
              end
            }
            body
          end
        end

        it "retrieves appropriate cookbooks#{num_versions ? ' with num_versions=' + num_versions.to_s : ''}" do
          url = if num_versions
                  api_url("/environments/#{env}/cookbooks?num_versions=#{num_versions}")
                else
                  api_url("/environments/#{env}/cookbooks")
                end
          get(url, admin_user) do |response|
            response.should look_like({
              :status => 200,
              :body_exact => expected_filtered_response(expected_pedant_cb_and_versions)
            })
          end
        end
      end
    end

    context 'with environment constraints' do

      test_with_constraints({'pedant_cb_one' => '= 1.0.0'},
                            {
        'pedant_cb_one' => ['1.0.0'],
        'pedant_cb_two' => ['1.2.5'],
        'pedant_cb_three' => ['1.0.0']
      })
      test_with_constraints({"pedant_cb_one" => '> 1.0.0'},
                            {
        'pedant_cb_one' => ['3.0.0'],
        'pedant_cb_two' => ['1.2.5'],
        'pedant_cb_three' => ['1.0.0']
      })
      test_with_constraints({"pedant_cb_one" => '> 1.0.0'},
                            {
        'pedant_cb_one' => ['3.0.0', '2.0.0'],
        'pedant_cb_two' => ['1.2.5', '1.2.0'],
        'pedant_cb_three' => ['1.0.0', '0.5.0']
      },
        2)
      # This is an odd response to me...
      test_with_constraints({"pedant_cb_one" => '> 1.0.0'},
                            {
        'pedant_cb_one' => [],
        'pedant_cb_two' => [],
        'pedant_cb_three' => []
      },
        0)
      test_with_constraints({
        'pedant_cb_one' => '> 1.0.0',
        'pedant_cb_two' => '< 1.2.3'
      },
        {
        'pedant_cb_one' => ['3.0.0', '2.0.0'],
        'pedant_cb_two' => ['1.2.0', '1.0.0'],
        'pedant_cb_three' => ['1.0.0', '0.5.0']
      },
        2)
      test_with_constraints({
        'pedant_cb_one' => '> 1.0.0',
        'pedant_cb_two' => '< 1.2.3'
      },
        {
        'pedant_cb_one' => ['3.0.0', '2.0.0'],
        'pedant_cb_two' => ['1.2.0', '1.0.0'],
        'pedant_cb_three' => ['1.0.0', '0.5.0', '0.0.1']
      },
        'all')
    end
  end
end # describe
