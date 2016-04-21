# Copyright: Copyright (c) 2015 Chef Software, Inc.
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
#

require 'pedant/rspec/cookbook_util'
require 'pedant/rspec/validations'

describe "Cookbooks API endpoint", :cookbooks, :cookbooks_create do

  let(:cookbook_url_base) { "cookbooks" }

  include Pedant::RSpec::CookbookUtil

  context "PUT /cookbooks/<name>/<version> [create]" do
    include Pedant::RSpec::Validations::Create
    let(:request_method){:PUT}
    let(:request_url){api_url("/#{cookbook_url_base}/#{cookbook_name}/#{cookbook_version}")}
    shared(:requestor){admin_user}

    let(:default_resource_attributes){ new_cookbook(cookbook_name, cookbook_version)}

    context 'with a basic cookbook', :smoke do
      after(:each) { delete_cookbook(admin_user, cookbook_name, cookbook_version) }

      let(:request_payload) { default_resource_attributes }
      let(:cookbook_name) { "pedant_basic" }
      let(:cookbook_version) { "1.0.0" }
      let(:created_resource) { default_resource_attributes }
      it "creates a basic cookbook" do
        should look_like created_exact_response
      end
    end

    # Start using the new validation macros
    context "when validating" do

      let(:cookbook_name) { "cookbook_name" }
      let(:cookbook_version) { "1.2.3" }

      let(:resource_url){api_url("/#{cookbook_url_base}/#{cookbook_name}/#{cookbook_version}")}
      let(:persisted_resource_response){ get(resource_url, requestor) }

      after(:each){ delete_cookbook(requestor, cookbook_name, cookbook_version)}

      context "the 'json_class' field" do
        let(:validate_attribute){"json_class"}
        accepts_valid_value "Chef::CookbookVersion"
        rejects_invalid_value "Chef::Node"
      end

      context "the cookbook version" do
        let(:request_payload) { default_resource_attributes }

        context "with negative versions", :validation do
          let(:cookbook_version) { "1.2.-42" }
          it { should look_like http_400_response }
        end

        context "with versions at exactly 4 bytes" do
          int4_exact = "2147483647"
          let(:cookbook_version) { "1.2.#{int4_exact}" }
          it { should look_like http_201_response }
        end

        context "with versions larger than 4 bytes" do
          int4_overflow = "2147483669" # max = 2147483647 (add 42)
          let(:cookbook_version) { "1.2.#{int4_overflow}" }
          it { should look_like http_201_response }
        end

        context "with versions larger than 8 bytes", :validation do
          int8_overflow = "9223372036854775849" # max = 9223372036854775807 (add 42)
          let(:cookbook_version) { "1.2.#{int8_overflow}" }
          it { should look_like http_400_response }
        end
      end

      rejects_invalid_keys

    end

    context "creating broken cookbooks to test validation and defaults", :validation do
      let(:cookbook_name) { "cookbook_name" }
      let(:cookbook_version) { "1.2.3" }

      malformed_constraint = "s395dss@#"

      context "basic tests" do
        after(:each) do
          delete_cookbook(admin_user, cookbook_name, cookbook_version)
        end

        should_create('json_class', :delete, true, 'Chef::CookbookVersion')
        should_fail_to_create('json_class', 'Chef::Role', 400, "Field 'json_class' invalid")
        should_fail_to_create('metadata', {}, 400, "Field 'metadata.version' missing")
      end # context basic tests

      context "checking segments" do
        %w{resources providers recipes definitions libraries attributes
           files templates root_files}.each do |segment|

          should_fail_to_create(segment, "foo", 400,
            "Field '#{segment}' invalid")
          should_fail_to_create(segment, [ {} ], 400,
            "Invalid element in array value of '#{segment}'.")
        end
      end # context checking segments

      context "checking metadata sections" do
        %w{platforms dependencies recommendations suggestions conflicting replacing}.each do |section|
          should_fail_to_create_metadata(section, "foo", 400, "Field 'metadata.#{section}' invalid")
          should_fail_to_create_metadata(section, {"foo" => malformed_constraint},
                                         400, "Invalid value '#{malformed_constraint}' for metadata.#{section}")
        end

        def self.should_create_with_metadata(_attribute, _value)
          context "when #{_attribute} is set to #{_value}" do
            let(:cookbook_name) { Pedant::Utility.with_unique_suffix("pedant-cookbook") }

            # These macros need to be refactored and updated for flexibility.
            # The cookbook endpoint uses PUT for both create and update, so this
            # throws a monkey wrench into the mix.
            should_change_metadata _attribute, _value, _value, 201
          end
        end

        context "with metadata.dependencies" do
          after(:each) do
            delete_cookbook(admin_user, cookbook_name, cookbook_version)
          end

          ["> 1.0", "< 2.1.2", "3.3", "<= 4.6", "~> 5.6.2", ">= 6.0"].each do |dep|
            should_create_with_metadata 'dependencies', {"chef-client" => "> 2.0.0", "apt" => dep}
          end

          ["> 1", "< 2", "3", "<= 4", "~> 5", ">= 6", "= 7", ">= 1.2.3.4", "<= 5.6.7.8.9.0"].each do |dep|
            should_fail_to_create_metadata(
              'dependencies',
              {"chef-client" => "> 2.0.0", "apt" => dep},
              400, "Invalid value '#{dep}' for metadata.dependencies")
          end
        end

        context "with metadata.providing" do
          after(:each) { delete_cookbook admin_user, cookbook_name, cookbook_version }

          # http://docs.chef.io/config_rb_metadata.html#provides
          should_create_with_metadata 'providing', 'cats::sleep'
          should_create_with_metadata 'providing', 'here(:kitty, :time_to_eat)'
          should_create_with_metadata 'providing', 'service[snuggle]'
          should_create_with_metadata 'providing', ''
          should_create_with_metadata 'providing', 1
          should_create_with_metadata 'providing', true
          should_create_with_metadata 'providing', ['cats', 'sleep', 'here']
          should_create_with_metadata 'providing',
          { 'cats::sleep'                => '0.0.1',
            'here(:kitty, :time_to_eat)' => '0.0.1',
            'service[snuggle]'           => '0.0.1'  }

        end
      end # context checking metadata sections

      context 'with invalid version in url' do
        let(:expected_response) { invalid_cookbook_version_response }
        let(:url) { named_cookbook_url }
        let(:payload) { {} }
        let(:cookbook_version) { 'abc' }

        it "should respond with an error" do
          put(url, admin_user, :payload => payload) do |response|
            response.should look_like expected_response
          end
        end # it invalid version in URL is a 400
      end # with invalid version in url

      it "invalid cookbook name in URL is a 400" do
        payload = {}
        put(api_url("/#{cookbook_url_base}/first@second/1.2.3"), admin_user,
            :payload => payload) do |response|
          error = "Invalid cookbook name 'first@second' using regex: 'Malformed cookbook name. Must only contain A-Z, a-z, 0-9, _, . or -'."
          response.should look_like({
                                      :status => 400,
                                      :body => {
                                        "error" => [error]
                                      }
                                    })
        end
      end # it invalid cookbook name in URL is a 400

      it "mismatched metadata.cookbook_version is a 400" do
        payload = new_cookbook(cookbook_name, "0.0.1")
        put(api_url("/#{cookbook_url_base}/#{cookbook_name}/#{cookbook_version}"), admin_user,
            :payload => payload) do |response|
          error = "Field 'name' invalid"
          response.should look_like({
                                      :status => 400,
                                      :body => {
                                        "error" => [error]
                                      }
                                    })
        end
      end # it mismatched metadata.cookbook_version is a 400

      it "mismatched cookbook_name is a 400" do
        payload = new_cookbook("foobar", cookbook_version)
        put(api_url("/#{cookbook_url_base}/#{cookbook_name}/#{cookbook_version}"), admin_user,
            :payload => payload) do |response|
          error = "Field 'name' invalid"
          response.should look_like({
                                      :status => 400,
                                      :body => {
                                        "error" => [error]
                                      }
                                    })
        end
      end # it mismatched cookbook_name is a 400

      context "sandbox checks" do
        after(:each) do
          delete_cookbook(admin_user, cookbook_name, cookbook_version)
        end
        it "specifying file not in sandbox is a 400" do
          payload = new_cookbook(cookbook_name, cookbook_version)
          payload["recipes"] = [
                                {
                                  "name" => "default.rb",
                                  "path" => "recipes/default.rb",
                                  "checksum" => "8288b67da0793b5abec709d6226e6b73",
                                  "specificity" => "default"
                                }
                               ]
          put(api_url("/#{cookbook_url_base}/#{cookbook_name}/#{cookbook_version}"),
              admin_user, :payload => payload) do |response|
            error = "Manifest has a checksum that hasn't been uploaded."
            response.should look_like({
                                        :status => 400,
                                        :body => {
                                          "error" => [error]
                                        }
                                      })
          end
        end # it specifying file not in sandbox is a 400
      end # context sandbox checks
    end # context creating broken cookbooks to test validation and defaults

    context "creating good cookbooks to test defaults" do
      let(:cookbook_name) { "cookbook_name" }
      let(:cookbook_version) { "1.2.3" }

      let(:description) { "my cookbook" }
      let(:long_description) { "this is a great cookbook" }
      let(:maintainer) { "This is my name" }
      let(:maintainer_email) { "cookbook_author@example.com" }
      let(:license) { "MPL" }

      let (:opts) {
        {
          :description => description,
          :long_description => long_description,
          :maintainer => maintainer,
          :maintainer_email => maintainer_email,
          :license => license
        }
      }

      after :each do
        delete_cookbook(admin_user, cookbook_name, cookbook_version)
      end

      respects_maximum_payload_size

      it "allows creation of a minimal cookbook with no data" do

        # Since PUT returns the same thing it was given, we'll just
        # define the input in terms of the response, since we use that
        # elsewhere in the test suite.
        payload = retrieved_cookbook(cookbook_name, cookbook_version)

        put(api_url("/#{cookbook_url_base}/#{cookbook_name}/#{cookbook_version}"),
            admin_user,
            :payload => payload) do |response|
          response.
            should look_like({
                               :status => 201,
                               :body => payload
                             })
        end
      end # it allows creation of a minimal cookbook with no data

      it "allows override of defaults" do
        payload = new_cookbook(cookbook_name, cookbook_version, opts)
        put(api_url("/#{cookbook_url_base}/#{cookbook_name}/#{cookbook_version}"),
            admin_user, :payload => payload) do |response|
          response.
            should look_like({
                               :status => 201,
                               :body => retrieved_cookbook(cookbook_name, cookbook_version,
                                                      opts)
                             })
        end
      end # it allows override of defaults
    end # context creating good gookbooks to test defaults
  end # context PUT /cookbooks/<name>/<version> [create]

  context "PUT multiple cookbooks" do
    let(:cookbook_name) { "multiple_versions" }
    let(:cookbook_version1) { "1.2.3" }
    let(:cookbook_version2) { "1.3.0" }

    after :each do
      [cookbook_version1, cookbook_version2].each do |v|
        delete(api_url("/#{cookbook_url_base}/#{cookbook_name}/#{v}"), admin_user)
      end
    end

    it "allows us to create 2 versions of the same cookbook" do
      payload = new_cookbook(cookbook_name, cookbook_version1, {})
      put(api_url("/#{cookbook_url_base}/#{cookbook_name}/#{cookbook_version1}"),
        admin_user,
        :payload => payload) do |response|
        response.should look_like({
            :status => 201,
            :body => payload
          })
      end

      payload2 = new_cookbook(cookbook_name, cookbook_version2, {})
      put(api_url("/#{cookbook_url_base}/#{cookbook_name}/#{cookbook_version2}"),
        admin_user,
        :payload => payload2) do |response|
        response.should look_like({
            :status => 201,
            :body => payload2
          })
      end

      get(api_url("/#{cookbook_url_base}/#{cookbook_name}/#{cookbook_version1}"),
        admin_user) do |response|
        response.should look_like({
                                   :status => 200,
                                   :body => retrieved_cookbook(cookbook_name, cookbook_version1)
                                  })
      end

      get(api_url("/#{cookbook_url_base}/#{cookbook_name}/#{cookbook_version2}"),
        admin_user) do |response|
        response.should look_like({
                                   :status => 200,
                                   :body => retrieved_cookbook(cookbook_name, cookbook_version2)
                                  })
      end
    end # it allows us to create 2 versions of the same cookbook
  end # context PUT multiple cookbooks
end # describe Cookbooks API endpoint
