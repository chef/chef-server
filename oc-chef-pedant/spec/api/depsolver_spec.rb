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

require 'pedant/rspec/cookbook_util'
require 'pedant/rspec/environment_util'

describe "Depsolver API endpoint", :depsolver do
  include Pedant::RSpec::CookbookUtil
  include Pedant::RSpec::EnvironmentUtil

  # Cookbook tests are parameterized to support common testing of both
  # /cookbooks and /cookbook_artifacts, so we need to specify that we want to
  # talk to /cookbooks. Have to define this as a method rather than a `let`
  # because we access it in `before(:all)` hooks
  def cookbook_url_base
    "cookbooks"
  end

  shared(:env){ "test_depsolver_env"}
  shared(:no_cookbooks_env) { "test_depsolver_no_cookbooks_env" }
  shared(:cookbook_name){"foo"}
  shared(:cookbook_version){"1.2.3"}
  let(:cookbook_name2) {"bar"}
  let(:cookbook_version2) {"2.0.0"}

  # We run all the depsolver tests in a newly created environment
  before(:all) {
    # Make sure we are testing an environment that has some
    # constraints even, if they don't actually constrain.
    the_env = new_environment(env)
    the_env['cookbook_versions'] = {
      'qux' => "> 4.0.0",
      'foo' => ">= 0.1.0",
      'bar' => "< 4.0.0"
    }
    add_environment(admin_user, the_env)

    no_cb_env = new_environment(no_cookbooks_env)
    no_cb_env['cookbook_versions'] = {
      'foo' => '= 400.0.0',
       'bar' => '> 400.0.0'
    }
    add_environment(admin_user, no_cb_env)
  }

  after(:all) {
    delete_environment(admin_user, env)
    delete_environment(admin_user, no_cookbooks_env)
  }

  context "POST /environments/:env/cookbook_versions" do

    context "empty and error cases" do
      before(:all) {
        make_cookbook(admin_user, cookbook_name, cookbook_version)
      }

      after(:all) {
        delete_cookbook(admin_user, cookbook_name, cookbook_version)
      }

      it "returns 400 with an empty payload", :validation do
        payload = ""
        post(api_url("/environments/#{env}/cookbook_versions"), admin_user,
            :payload => payload) do |response|
          response.should look_like({
                                     :status => 400,
                                     :body_exact => {
                                         "error" => ["invalid JSON"]
                                     }
                                    })
        end
      end

      it "returns 400 with an non-json payload", :validation do
        payload = "this_is_not_json"
        post(api_url("/environments/#{env}/cookbook_versions"), admin_user,
            :payload => payload) do |response|
          response.should look_like({
                                     :status => 400,
                                     :body_exact => {
                                         "error" => ["invalid JSON"]
                                     }
                                    })
        end
      end

      let(:environment_name){"not@environment"}

      it "returns 404 with an invalid environment" do
        payload = "{\"run_list\":[]}"
        post(api_url("/environments/#{environment_name}/cookbook_versions"), normal_user,
             :payload => payload) do |response|
          response.should look_like environment_not_found_response
        end
      end

      it "returns 400 with non-Array as run_list value", :validation do
        payload = "{\"run_list\":\"#{cookbook_name}\"}"
        post(api_url("/environments/#{env}/cookbook_versions"), admin_user,
            :payload => payload) do |response|
          response.should look_like({
                                     :status => 400,
                                     :body_exact => {
                                         "error" => ["Field 'run_list' is not a valid run list"]
                                     }
                                    })
        end
      end

      it "returns 400 with malformed JSON", :validation do
        payload = "{\"run_list\": "
        post(api_url("/environments/#{env}/cookbook_versions"), admin_user,
              :payload => payload) do |response|
          response.should look_like({
                                      :status => 400,
                                      :body_exact => {
                                        "error" => ["invalid JSON"]
                                      }
                                    })
        end
      end

      it "returns Error with an malformed item in run_list (int)", :validation do
        payload = "{\"run_list\": [12]}"
        post(api_url("/environments/#{env}/cookbook_versions"), admin_user,
            :payload => payload) do |response|
          response.should look_like({
                                      :status => 400,
                                      :body_exact => {
                                        "error" => ["Field 'run_list' is not a valid run list"]
                                      }
                                    })
        end
      end

      it "returns 200 with an empty run_list" do
        payload = "{\"run_list\":[]}"
        post(api_url("/environments/#{env}/cookbook_versions"), admin_user,
            :payload => payload) do |response|
          response.should look_like({
                                     :status => 200,
                                     :body_exact => {
                                     }

                                    })
        end
      end

      it "returns 412 with a non-existent cookbook in _default environment" do
        not_exist = "this_does_not_exist"
        payload = "{\"run_list\":[\"#{not_exist}\"]}"
        error_hash = {
          "message" => "Run list contains invalid items: no such cookbook #{not_exist}.",
          "non_existent_cookbooks" => [ not_exist ],
          "cookbooks_with_no_versions" => []
        }
        post(api_url("/environments/_default/cookbook_versions"), admin_user,
             :payload => payload) do |response|
          response.should look_like({
                                      :status => 412,
                                      :body_exact => {
                                        "error" => [error_hash]
                                      }
                                    })
        end
      end

      it "returns 412 with a non-existent cookbook" do
        not_exist = "this_does_not_exist"
        payload = "{\"run_list\":[\"#{not_exist}\"]}"
        error_hash = {
          "message" => "Run list contains invalid items: no such cookbook #{not_exist}.",
          "non_existent_cookbooks" => [ not_exist ],
          "cookbooks_with_no_versions" => []
        }
        post(api_url("/environments/#{env}/cookbook_versions"), admin_user,
             :payload => payload) do |response|
          response.should look_like({
                                      :status => 412,
                                      :body_exact => {
                                        "error" => [error_hash]
                                      }
                                    })
        end
      end

      it "returns 412 with an existing cookbook filtered out by environment" do
        payload = "{\"run_list\":[\"#{cookbook_name}\"]}"
        error_hash = {
          "message" => "Run list contains invalid items: no versions match the constraints on cookbook (foo >= 0.0.0).",
          "non_existent_cookbooks" => [],
          "cookbooks_with_no_versions" => ["(foo >= 0.0.0)"]
        }
        post(api_url("/environments/#{no_cookbooks_env}/cookbook_versions"), admin_user,
             :payload => payload) do |response|
          response.should look_like({
                                      :status => 412,
                                      :body_exact => {
                                        "error" => [error_hash]
                                      }
                                    })
        end
      end

      it "returns 412 and both cookbooks with more than one non-existent cookbook" do
        not_exist1 = "this_does_not_exist"
        not_exist2 = "also_this_one"
        payload = "{\"run_list\":[\"#{not_exist1}\", \"#{not_exist2}\"]}"
        error_hash = {
          "message" => "Run list contains invalid items: no such cookbooks #{not_exist1}, #{not_exist2}.",
          "non_existent_cookbooks" => [ not_exist1, not_exist2 ],
          "cookbooks_with_no_versions" => []
        }
        post(api_url("/environments/#{env}/cookbook_versions"), admin_user,
             :payload => payload) do |response|
          response.should look_like({
                                      :status => 412,
                                      :body_exact => {
                                        "error" => [error_hash]
                                      }
                                    })
        end
      end

      it "returns 412 when there is a runlist entry specifying version that doesn't exist" do
        missing_version_payload = "{\"run_list\":[\"#{cookbook_name}@#{cookbook_version2}\"]}"
        error_hash = {
          "message" => "Run list contains invalid items: no versions match the constraints on cookbook (#{cookbook_name} = #{cookbook_version2}).",
          "non_existent_cookbooks" => [],
          "cookbooks_with_no_versions" => ["(#{cookbook_name} = #{cookbook_version2})"]
        }
        post(api_url("/environments/#{env}/cookbook_versions"), admin_user,
             :payload => missing_version_payload) do |response|
          response.should look_like({
                                      :status => 412,
                                      :body_exact => {
                                        "error" => [error_hash]
                                      }
                                    })
        end
      end


      # TODO: See if it is possible to get both errors out for non-existent runlist items and
      # cookbooks which have no versions matching constraints
      it "returns 412 and both errors non-existent and no versions cookbooks" do
        not_exist = "this_does_not_exist"
        payload = "{\"run_list\":[\"#{not_exist}\", \"#{cookbook_name}@2.0.0\"]}"
        error_hash = {
          "message" => "Run list contains invalid items: no such cookbook #{not_exist}.",
          "non_existent_cookbooks" => [ not_exist ],
          "cookbooks_with_no_versions" => []
        }
        post(api_url("/environments/#{env}/cookbook_versions"), admin_user,
             :payload => payload) do |response|
          response.should look_like({
                                      :status => 412,
                                      :body_exact => {
                                        "error" => [error_hash]
                                      }
                                    })
        end
      end

    end # empty and error cases context

    context "dependency error cases (one cookbook)" do
      let(:payload) {"{\"run_list\":[\"#{cookbook_name}\"]}"}

      # We create the cookbook in the test with a specific dependency.
      # Clean it up here
      after(:each) {
        delete_cookbook(admin_user, cookbook_name, cookbook_version)
      }

      # TODO: need more detailed depsolver output to construct error message
      it "returns 412 when there is a dep that doesn't exist" do
        not_exist_name = "this_does_not_exist"
        not_exist_version = "0.0.0"
        opts = { :dependencies => {not_exist_name => ">= #{not_exist_version}"}}
        make_cookbook(admin_user, cookbook_name, cookbook_version,opts)
        error_hash = {
          "message" => "Unable to satisfy constraints on package this_does_not_exist, " +
                       "which does not exist, due to solution constraint (foo >= 0.0.0). " +
                       "Solution constraints that may result in a constraint on this_does_not_exist: " +
                       "[(foo = 1.2.3) -> (this_does_not_exist >= 0.0.0)]",
           "unsatisfiable_run_list_item" => "(foo >= 0.0.0)",
          "non_existent_cookbooks" => ["this_does_not_exist"],
          "most_constrained_cookbooks" => []
        }
        post(api_url("/environments/#{env}/cookbook_versions"), admin_user,
             :payload => payload) do |response|
          response.should look_like({
                                      :status => 412,
                                      :body_exact => {
                                        "error" => [error_hash]
                                      }
                                    })
        end
      end
    end # dependency error cases (one cookbook)

    context "dependency error cases (two cookbooks)" do
      let(:payload) {"{\"run_list\":[\"#{cookbook_name}\"]}"}

      # We create the cookbooks in the test with a specific dependency.
      # Clean them up here
      after(:each) {
        delete_cookbook(admin_user, cookbook_name, cookbook_version)
        delete_cookbook(admin_user, cookbook_name2, cookbook_version2)
      }

      it "returns 412 and both entries when there are runlist entries specifying versions that don't exist" do
        make_cookbook(admin_user, cookbook_name, cookbook_version)
        make_cookbook(admin_user, cookbook_name2, cookbook_version2)
        missing_version_payload = "{\"run_list\":[\"#{cookbook_name2}@2.0.0\", \"#{cookbook_name}@3.0.0\"]}"
        error_hash = {
          "message" => "Run list contains invalid items: no versions match the constraints on cookbook (foo = 3.0.0).",
          "non_existent_cookbooks" => [],
          "cookbooks_with_no_versions" => ["(foo = 3.0.0)"]
        }
        post(api_url("/environments/#{env}/cookbook_versions"), admin_user,
             :payload => missing_version_payload) do |response|
          response.should look_like({
                                      :status => 412,
                                      :body_exact => {
                                        "error" => [error_hash]
                                      }
                                    })
        end
      end

      # TODO: need more detailed depsolver output to construct error message
      it "returns 412 when there is a dep that doesn't have new enough version" do
        opts = { :dependencies => {cookbook_name2 => "> #{cookbook_version2}"} }
        make_cookbook(admin_user, cookbook_name, cookbook_version, opts)
        make_cookbook(admin_user, cookbook_name2, cookbook_version2)
        error_hash = {
          "message" =>
            "Unable to satisfy constraints on package bar due to solution constraint " +
            "(foo >= 0.0.0). Solution constraints that may result in a constraint on bar: " +
            "[(foo = 1.2.3) -> (bar > 2.0.0)]",
          "unsatisfiable_run_list_item" => "(foo >= 0.0.0)",
          "non_existent_cookbooks" => [],
          "most_constrained_cookbooks" => ["bar = 2.0.0 -> []"]
        }
        post(api_url("/environments/#{env}/cookbook_versions"), admin_user,
             :payload => payload) do |response|
          response.should look_like({
                                      :status => 412,
                                      :body_exact => {
                                        "error" => [error_hash]
                                      }
                                    })
        end
      end

      # TODO: need more detailed depsolver output to construct error message
      it "returns 412 when there is an impossible dependency" do
        opts1 = { :dependencies => {cookbook_name2=>"> 2.0.0"}}
        opts2 = { :dependencies => {cookbook_name=>"> 3.0.0"}}
        make_cookbook(admin_user, cookbook_name, cookbook_version, opts1)
        make_cookbook(admin_user, cookbook_name2, cookbook_version2, opts2)
        error_hash = {
          "message" => "Unable to satisfy constraints on package bar due to " +
                       "solution constraint (foo >= 0.0.0). Solution constraints " +
                       "that may result in a constraint on bar: [(foo = 1.2.3) -> (bar > 2.0.0)]",
          "unsatisfiable_run_list_item" => "(foo >= 0.0.0)",
          "non_existent_cookbooks" => [],
          "most_constrained_cookbooks" => ["bar = 2.0.0 -> [(foo > 3.0.0)]"]
        }
        post(api_url("/environments/#{env}/cookbook_versions"), admin_user,
             :payload => payload) do |response|
          response.should look_like({
                                      :status => 412,
                                      :body_exact => {
                                        "error" => [error_hash]
                                      }
                                    })
        end
      end

    end # dependency error cases (two cookbooks)

    context "success cases" do
      let(:payload) {"{\"run_list\":[\"#{cookbook_name}\"]}"}

      before(:all) {
        make_cookbook(admin_user, cookbook_name, cookbook_version)
      }

      after(:all) {
        delete_cookbook(admin_user, cookbook_name, cookbook_version)
      }

      it "returns 200 with a minimal good cookbook", :smoke do

        cb = retrieved_cookbook(cookbook_name, cookbook_version)

        meta = cb["metadata"]
        meta.delete("attributes")
        meta.delete("long_description")
        cb["metadata"] = meta

        post(api_url("/environments/#{env}/cookbook_versions"), normal_user,
              :payload => payload) do |response|
          response.should look_like({
                                      :status => 200,
                                      :body => {
                                        cookbook_name => cb
                                      }
                                    })
        end
      end

      context "CHEF-3813: Return cookbook dependency metadata when a cookbook has dependencies" do

        before :all do
          make_cookbook(admin_user, "foo", "1.0.0")
          make_cookbook(admin_user, "bar", "2.0.0", {:dependencies => {"foo" => "> 0.0.0"}})
          make_cookbook(admin_user, "baz", "3.0.0")
          make_cookbook(admin_user, "quux", "4.0.0", {:dependencies => {"bar" => "= 2.0.0", "baz" => "= 3.0.0"}})
        end

        after :all do
          delete_cookbook(admin_user, "foo", "1.0.0")
          delete_cookbook(admin_user, "bar", "2.0.0")
          delete_cookbook(admin_user, "baz", "3.0.0")
          delete_cookbook(admin_user, "quux", "4.0.0")
        end

        it "returns dependencies" do
          post(api_url("/environments/_default/cookbook_versions"), normal_user,
               :payload => {"run_list" => ["quux"]}) do |response|

            response.should have_status_code 200

            json = parse(response)

            json["foo"]["metadata"]["dependencies"].should eq({})
            json["bar"]["metadata"]["dependencies"].should eq({"foo" => "> 0.0.0"})
            json["baz"]["metadata"]["dependencies"].should eq({})
            json["quux"]["metadata"]["dependencies"].should eq({"bar" => "= 2.0.0", "baz" => "= 3.0.0"})
          end
        end
      end

      context "with datestamps in cookbooks and environments" do
        before :each do
          make_cookbook(admin_user, "datestamp", "1.2.20130730201745")
          datestamp_env = new_environment("datestamp_env")
          datestamp_env['cookbook_versions'] = {
            'datestamp' => ">= 1.2.20130730200000"
          }
          add_environment(admin_user, datestamp_env)
        end

        after :each do
          delete_cookbook(admin_user, "datestamp", "1.2.20130730201745")
          delete_environment(admin_user, "datestamp_env")
        end

        it "returns the correct solution" do
          post(api_url("/environments/datestamp_env/cookbook_versions"), admin_user,
               :payload => {"run_list" => ["datestamp"]}) do |response|
            response.should have_status_code 200
          end
        end
      end

      # TODO: Check with a cookbook that has some files in segments since we also get
      # back URLs
    end # success cases context
  end # global context
end # describe
