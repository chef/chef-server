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


describe "/environments/ENVIRONMENT/cookbooks/COOKBOOK API endpoint", :environments, :cookbooks do
  include Pedant::RSpec::CookbookUtil
  include Pedant::RSpec::EnvironmentUtil

  # Cookbook tests are parameterized to support common testing of both
  # /cookbooks and /cookbook_artifacts, so we need to specify that we want to
  # talk to /cookbooks
  let(:cookbook_url_base) { "cookbooks" }

  include_context "environment_body_util" # from EnvironmentUtil

  # TODO: Refactor as macros instead of being data-driven
  def self.env
    "test_env"
  end

  def self.default
    "_default"
  end

  # TODO: Refactor as macros instead of being data-driven
  # Ditto for cookbooks
  def self.cookbooks
    {
      "cb_one" => {
      "1.0.0" => ['webserver'],
      "2.0.0" => ['database', 'webserver'],
      "3.0.0" => ['awesome_sauce', 'database', 'webserver']
    },
      "cb_two" => {
      "1.0.0" => ['chicken'],
      "1.2.0" => ['beef', 'chicken'],
      "1.2.5" => ['beef', 'chicken', 'stewed_monkey_brains']
    },
      "cb_three" => {
      "0.5.1" => ['server'],
      "0.6.0" => ['client', 'server'],
      "1.0.0" => ['client', 'replication', 'server']
    }
    }
  end

  let(:env)       { self.class.env }
  let(:default)   { self.class.default }
  let(:cookbooks) { self.class.cookbooks }

  before(:each) { add_environment(admin_user, new_environment(env)) }
  after(:each)  { delete_environment(admin_user, env) }

  context "with no cookbooks" do
    it "fails if the environment does not exist" do
      non_existent_environment = 'bad_env'
      non_existent_environment.should_not eq env

      get(api_url("/environments/#{non_existent_environment}/cookbooks/fake_cookbook_doesnt_matter"), admin_user) do |response|
        response.should look_like(
                                    {
                                      :status => 404,
                                      :body_exact => {
                                        "error" => ["Cannot load environment #{non_existent_environment}"]
                                      }
                                    }
                                  )
      end
    end

    [env, default].each do |environment|
      it "does not find a non-existent cookbook in the '#{environment}' cookbook" do
        non_existent_cookbook = "non_existent_cookbook"

        # Verify the cookbook doesn't exist
        # (searching for the _latest version as a way to query for existence of the cookbook)
        get(api_url("/cookbooks/#{non_existent_cookbook}/_latest"), admin_user) do |response|
          # Ruby bombs out when checking for a non-existent cookbook
          response.should have_status_code(404)
        end

        get(api_url("/environments/#{environment}/cookbooks/#{non_existent_cookbook}"), admin_user) do |response|
          response.should look_like(
                                      {
                                        :status => 404,
                                        :body_exact => {
                                          "error" => ["Cannot find a cookbook named #{non_existent_cookbook}"]
                                        }
                                      }
                                    )
        end
      end
    end
  end

  context 'with multiple versions of multiple cookbooks' do

    before :each do
      setup_cookbooks(cookbooks)
    end

    after :each do
      remove_cookbooks(cookbooks)
    end

    # Generate the expected JSON body from a set of cookbook specs for
    # a call to the /environments/ENVIRONMENT/cookbooks/COOKBOOK endpoint
    def expected_for_cookbooks(cookbooks, cookbook_name, num_versions)
      latest = get_latest_cookbooks(cookbooks, num_versions)
      latest.inject({}) do |body, cookbook_spec|
        name, version_specs  = cookbook_spec

        if (name == cookbook_name)
          body[name] = {
            "url" => api_url("/cookbooks/#{name}"),
            "versions" => version_specs.map do |version_string, recipe_names|
              {
                "url" => api_url("/cookbooks/#{name}/#{version_string}"),
                "version" => version_string
              }
            end
            }
        end

        body
      end
    end

    context 'with no environment constraints' do
      [env, default].each do |environment|
        cookbooks.keys.each do |cookbook|
          [nil, 1, 2, 3, 30, 'all'].each do |num_versions|

            description = if num_versions
                            "returns #{num_versions} acceptable cookbook version(s) of #{cookbook} from the '#{environment}' environment"
                          else
                            "returns all acceptable cookbook version(s) of #{cookbook} from the '#{environment}' environment when num_versions is not specified"
                          end

            it description do
              url = if num_versions
                      api_url("/environments/#{environment}/cookbooks/#{cookbook}?num_versions=#{num_versions}")
                    else
                      api_url("/environments/#{environment}/cookbooks/#{cookbook}")
                    end
              get(url, admin_user) do |response|
                response.should look_like({
                                            :status => 200,
                                            :body_exact => expected_for_cookbooks(cookbooks, cookbook, num_versions ? num_versions : "all") # should default to "all"
                                          })
              end
            end # it
          end #numversions
        end # cookbooks
      end # env
    end # with no environment constraints context





    def self.test_with_constraints(cookbook, cookbook_constraint, expected_results_for_num_versions)

      constraint_hash = {cookbook => cookbook_constraint}

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
        # =>  passing in a data structure that represents the basics of
        # the response because I don't feel like recapitulating the
        # environment filtering logic for a test helper function :)
        def expected_filtered_response(cookbook, allowed_versions)
          body = {}
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

        expected_results_for_num_versions.each do |num_versions,v|
          it "retrieves appropriate cookbook versions of '#{cookbook}' with num_versions=#{num_versions}" do
            get(api_url("/environments/#{env}/cookbooks/#{cookbook}?num_versions=#{num_versions}"), admin_user) do |response|
              response.should look_like({
                                          :status => 200,
                                          :body_exact => expected_filtered_response(cookbook, v)
                                        })
            end #get
          end # it

          if num_versions == 'all'
            it "retrieves all appropriate cookbook versions of '#{cookbook}' without 'num_versions'" do
              get(api_url("/environments/#{env}/cookbooks/#{cookbook}"), admin_user) do |response|
                response.should look_like({
                                            :status => 200,
                                            :body_exact => expected_filtered_response(cookbook, v)
                                        })
              end #get
            end # it
          end # if



        end # expected results
      end # inner context
    end #self.test_with_constraints

    context 'with environment constraints' do
      test_with_constraints("cb_one",
                            '= 1.0.0',
                            {
                              '1' => ['1.0.0'],
                              '2' => ['1.0.0'],
                              '3' => ['1.0.0'],
                              'all' => ['1.0.0']
                            })
      test_with_constraints("cb_one",
                            '> 1.0.0',
                            {
                              '1' => ['3.0.0'],
                              '2' => ['3.0.0', '2.0.0'],
                              '3' => ['3.0.0', '2.0.0'],
                              'all' => ['3.0.0', '2.0.0']
                            })
      test_with_constraints("cb_one",
                            '= 6.6.6',
                            {
                              '1' => [],
                              '2' => [],
                              '3' => [],
                              'all' => []
                            })
      test_with_constraints("cb_three",
                            '~> 0.5',
                            {
                              '1' => ['0.6.0'],
                              '2' => ['0.6.0', '0.5.1'],
                              '3' => ['0.6.0', '0.5.1'],
                              'all' => ['0.6.0', '0.5.1']
                            })


    end # with environment constraints context
  end # with multiple versions of multiple cookbooks
end # everything
