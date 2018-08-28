# Copyright: Copyright 2015-2018 Chef Software, Inc.
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

require 'pedant/rspec/cookbook_util'

describe "Cookbooks API endpoint, named filters", :cookbooks, :cookbooks_named_filters do

  shared_examples "filters cookbooks" do |api_version|
    let(:cookbook_url_base) { "cookbooks" }
    let(:api_version) { api_version }

    before do
      platform.server_api_version = api_version
    end

    after do
      platform.reset_server_api_version
    end

    let(:segment_type) do
      if api_version.to_i < 2
        "files"
      else
        "all_files"
      end
    end

    include Pedant::RSpec::CookbookUtil

    let(:request_method) { :GET }
    let(:request_url)    { api_url "/#{cookbook_url_base}/#{named_filter}" }
    let(:requestor)      { admin_user }

    # Generates a hash of cookbook name -> cookbook version url for the
    # most recent version of each cookbook in a cookbook spec
    def expected_for_latest(cookbooks)
      latest = get_latest_cookbooks(cookbooks)
      latest.inject({}) do |body, cookbook_spec|
        name, version_specs  = cookbook_spec
        latest_version = version_specs.first
        version_string, _recipe_names = latest_version
        body[name] = api_url("/#{cookbook_url_base}/#{name}/#{version_string}")
        body
      end
    end

    def expected_for_named_cookbook(cookbooks, named_cookbook)
      cookbook = cookbooks.select{ |k,v| k == named_cookbook}


      cookbook.inject({}) do |body, cookbook_spec|
        name, version_specs  = cookbook_spec
        body[name] = {
          "url" => api_url("/#{cookbook_url_base}/#{name}"),
          "versions" => version_specs.map do |version, recipes|
            { "version" => version,
              "url" => api_url("/#{cookbook_url_base}/#{name}/#{version}")
            }
          end
        }
        body
      end
    end

    # Generates a list of cookbook-qualified recipe names for the most
    # recent cookbooks in a cookbook spec
    def expected_for_recipes(cookbooks)
      latest = get_latest_cookbooks(cookbooks)
      nested = latest.map do |cookbook_spec|
        cookbook_name, version_specs = cookbook_spec
        latest_version = version_specs.first
        _version_string, recipe_names =latest_version

        # don't sort the recipes for the Ruby endpoint; CouchDB keeps them in insertion order
        recipe_names.sort!

        recipe_names.map do |recipe_name|
          "#{cookbook_name}::#{recipe_name}"
        end
      end
      nested.flatten
    end

    let(:cookbooks) { raise "Must define a cookbook spec!" }
    let(:cookbook_name) {"my_cookbook"}

    # Changed from before(:all) to avoid some problems, but it slows down these tests
    before(:each) { setup_cookbooks(cookbooks) }
    after(:each)  { remove_cookbooks(cookbooks) }

    # All these tests are basically the same, and just have different
    # cookbooks in the system, so I'm pulling it all out into a shared
    # set of examples
    def self.should_respond_with_latest_cookbooks
      context 'when requesting /cookbooks/_latest' do
        let(:named_filter) { :_latest }
        let(:expected_response) { fetch_cookbook_success_exact_response }
        let(:fetched_cookbook) { expected_for_latest(cookbooks) }

        it "should respond with the 'latest' cookbooks" do
          should look_like expected_response
        end
      end
    end

    def self.should_respond_with_recipes_from_latest_cookbooks
      context 'when requesting /cookbooks/_recipes' do
        let(:named_filter) { :_recipes }
        let(:expected_response) { fetch_cookbook_success_exact_response }

        it "shows the recipes from the latest cookbooks" do
          should have_status_code 200
          parse(response).should == expected_for_recipes(cookbooks)
        end
      end
    end

    def self.should_respond_with_single_cookbook
      context 'when requesting /cookbooks/my_cookbook' do
        let(:cookbook) { "my_cookbook" }
        let(:named_filter) { "my_cookbook" }
        let(:fetched_cookbook) { expected_for_named_cookbook(cookbooks, named_filter) }
        let(:expected_response) { fetch_cookbook_success_exact_response }

        it "should respond with the 'named' cookbook" do
          should look_like expected_response
        end
      end
    end

    def self.should_respond_with_single_cookbook_not_found
      context 'when requesting /cookbooks/my_cookbook' do
        let(:named_filter) { "my_cookbook" }
        let(:expected_response) { fetch_cookbook_not_found_exact_response }

        it "should respond with 404" do
          should look_like expected_response
        end
      end
    end



    ## Now for the actual tests

    context "with no cookbooks" do
      let(:cookbooks) { {} }

      should_respond_with_latest_cookbooks
      should_respond_with_recipes_from_latest_cookbooks
      should_respond_with_single_cookbook_not_found
    end

    # TODO: This would be a good candidate for a smoke test
    # if there were a way to turn off testing against exact body responses.
    # We don't know what existing cookbooks are on the target server for
    # smoke tests.
    context "with one cookbook, one version" do
      let(:cookbooks) do
        {"my_cookbook" => {"1.0.0" => ["recipe1", "recipe2"]}}
      end

      should_respond_with_latest_cookbooks
      should_respond_with_recipes_from_latest_cookbooks
      should_respond_with_single_cookbook
    end

    context "with different cookbook, one version" do
      let(:cookbooks) do
        {"your_cookbook" => {"1.0.0" => ["recipe1", "recipe2"]}}
      end

      should_respond_with_latest_cookbooks
      should_respond_with_recipes_from_latest_cookbooks
      should_respond_with_single_cookbook_not_found
    end

    context "with multiple cookbooks, one version each" do
      let(:cookbooks) do
        {
          "my_cookbook" => {"1.0.0" => ["recipe1", "recipe2"]},
          "your_cookbook" => {"1.3.0" => ["foo", "bar"]},
        }
      end

      should_respond_with_latest_cookbooks
      should_respond_with_recipes_from_latest_cookbooks
      should_respond_with_single_cookbook
    end

    context "with multiple cookbooks, multiple versions each" do
      let(:cookbooks) do
        {
          "my_cookbook" => {
            "1.0.0" => ["monitoring", "security"],
            "1.5.0" => ["security", "users"]
          },
          "your_cookbook" => {
            "1.3.0" => ["webserver", "database"],
            "2.0.0" => ["webserver", "database", "load_balancer"]
          },
        }
      end

      should_respond_with_latest_cookbooks
      should_respond_with_recipes_from_latest_cookbooks
      should_respond_with_single_cookbook
    end

  end
  describe "API v0" do
    it_behaves_like "filters cookbooks", 0
  end

  describe "API v2" do
    it_behaves_like "filters cookbooks", 2
  end
end
