# Copyright: Copyright (c) 2015-2020, Chef Software Inc.
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

describe "Cookbook Artifacts API endpoint", :cookbook_artifacts, :cookbook_artifacts_create do

  shared_examples "creates cookbook artifacts" do |api_version|
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

    let(:cookbook_url_base) { "cookbook_artifacts" }

    let(:default_cookbook_id) { "1111111111111111111111111111111111111111" }

    include Pedant::RSpec::CookbookUtil

    let(:default_version) { "1.0.0" }

    context "PUT /cookbook_artifacts/<name>/<version> [create]" do

      include Pedant::RSpec::Validations::Create

      let(:request_method){:PUT}
      let(:request_url){api_url("/#{cookbook_url_base}/#{cookbook_name}/#{cookbook_identifier}")}
      shared(:requestor){admin_user}

      let(:cookbook_artifact_to_create){ new_cookbook_artifact(cookbook_name, cookbook_identifier)}

      context 'with a basic cookbook', :smoke do
        after(:each) { delete_cookbook_artifact(admin_user, cookbook_name, cookbook_identifier) }

        let(:cookbook_name) { "pedant_basic" }
        let(:cookbook_identifier) { "1111111111111111111111111111111111111111" }

        let(:expected_get_response_data) do
          new_cookbook_artifact(cookbook_name, cookbook_identifier, version: "1.0.0")
        end

        it "creates a basic cookbook_artifact" do
          # create it:
          create_response = put(request_url, requestor, payload: cookbook_artifact_to_create)
          expect(create_response.code.to_s).to eq("201")

          # list:
          list_response = get(api_url("/#{cookbook_url_base}/#{cookbook_name}"), requestor)

          list_data = parse(list_response)
          expect(list_data).to have_key("pedant_basic")
          expect(list_data["pedant_basic"]["versions"].size).to eq(1)
          expect(list_data["pedant_basic"]["versions"].first["identifier"]).to eq(cookbook_identifier)

          # fetch it:
          get_response = get(request_url, requestor)

          expect(get_response.code.to_s).to eq("200")
          expect_matching_cookbook_artifact(parse(get_response), expected_get_response_data)
        end
      end

      # Start using the new validation macros
      context "when validating" do

        let(:cookbook_name) { "cookbook_name" }
        let(:cookbook_identifier) { "1111111111111111111111111111111111111111" }

        let(:resource_url){api_url("/#{cookbook_url_base}/#{cookbook_name}/#{cookbook_identifier}")}
        let(:persisted_resource_response){ get(resource_url, requestor) }

        after(:each){ delete_cookbook_artifact(requestor, cookbook_name, cookbook_identifier)}

        context "the cookbook version" do

          let(:cookbook_artifact_to_create) do
            new_cookbook_artifact(cookbook_name, cookbook_identifier, version: cookbook_version)
          end

          let(:request_payload) { cookbook_artifact_to_create }
          let(:default_resource_attributes) { cookbook_artifact_to_create }

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

          context "with versions with 'prerelease' fields" do

            let(:cookbook_version) { "1.2.3.beta.5" }

            it { should look_like http_201_response }
          end
        end

      end

      context "creating broken cookbook_artifacts to test validation and defaults", :validation do
        let(:cookbook_name) { "cookbook_name" }
        let(:cookbook_version) { "1.2.3" }

        let(:cookbook_identifier) { default_cookbook_id }
        let(:base_payload) do
          new_cookbook_artifact(cookbook_name, cookbook_identifier, version: cookbook_version).tap do |ca|
            ca.delete("json_class")
          end
        end

        # delete b/c some tests actually do succeed w/ create or to clean up
        # after failed negative test.
        after(:each) { delete_cookbook_artifact(admin_user, cookbook_name, cookbook_identifier) }

        def update_payload(key, value)
          base_payload.dup.tap do |payload|
            if (value == :delete)
              payload.delete(key)
            else
              payload[key] = value
            end
          end
        end

        shared_examples_for "valid_cookbook_artifact" do

          it "create returns 201" do
            #create it
            put(request_url, requestor, :payload => payload) do |response|
              expect(response.code).to eq(201)
            end

            # verify it looks correct
            get(request_url, requestor) do |response|
              response.
                should look_like({
                :status => 200,
                :body => payload
              })
            end
          end
        end

        shared_examples_for "invalid_cookbook_artifact" do
          it "create returns 400" do
            # create should respond with error
            put(request_url, requestor, :payload => payload) do |response|
              expect(response).to look_like({
                :status => 400,
                :body_exact => {"error" => [error_message]}
              })
            end

            # double check it did't get created
            get(request_url, requestor) do |response|
              expect(response).to look_like({
                :status => 404
              })
            end
          end
        end

        context "basic tests" do
          after(:each) do
            delete_cookbook_artifact(admin_user, cookbook_name, cookbook_identifier)
          end

          context "with empty metadata", skip: "FIXME - server needs to add validation" do
            let(:payload) { update_payload("metadata", {}) }
            let(:error_message) { "Field 'metadata.version' missing" }

            include_examples("invalid_cookbook_artifact")
          end
        end # context basic tests

        segment_names = %w{resources providers recipes definitions libraries attributes files templates root_files}
        segment_names.each do |segment|

          context "with segment '#{segment}' set to a String", skip: "FIXME - server returns 500" do

            let(:payload) { update_payload(segment, "foo") }
            let(:error_message) { "Field '#{segment}' invalid" }

            include_examples("invalid_cookbook_artifact")
          end

          context "with segment '#{segment}' set to an Array with one empty JSON object", skip: "FIXME - server returns 500" do

            let(:payload) { update_payload(segment, [ {} ]) }
            let(:error_message) { "Invalid element in array value of '#{segment}'." }

            include_examples("invalid_cookbook_artifact")

          end
        end # context checking segments

        context "checking metadata sections" do

          def update_payload_metadata(key, value)
            base_payload.dup.tap do |payload|
              payload["metadata"] = payload["metadata"].dup.tap do |md|
                if (value == :delete)
                  md.delete(key)
                else
                  md[key] = value
                end
              end
            end
          end

          let(:malformed_constraint) { "s395dss@#" }

          %w{platforms dependencies}.each do |section|

            context "with metadata section '#{section}' set to a string", skip: "FIXME - missing server validation" do

              let(:payload) { update_payload_metadata(section, "foo") }
              let(:error_message) { "Field 'metadata.#{section}' invalid" }

              include_examples("invalid_cookbook_artifact")

            end

            context "with a malformed constraint for metadata section #{section}", skip: "FIXME - missing server validation" do

              let(:payload) { update_payload_metadata(section, {"foo" => malformed_constraint}) }
              let(:error_message) { "Invalid value #{malformed_constraint} for metadata.#{section}" }

              include_examples("invalid_cookbook_artifact")
            end

          end


          ["> 1.0", "< 2.1.2", "3.3", "<= 4.6", "~> 5.6.2", ">= 6.0"].each do |dep|

            context "with valid metadata dependency '#{dep}'" do
              let(:payload) do
                update_payload_metadata("dependencies", {"chef-client" => "> 2.0.0", "apt" => dep})
              end

              include_examples("valid_cookbook_artifact")

            end
          end

          ["> 1", "< 2", "3", "<= 4", "~> 5", ">= 6", "= 7", ">= 1.2.3.4", "<= 5.6.7.8.9.0"].each do |dep|

            context "with invalid metadata dependency '#{dep}'", skip: "FIXME - missing server validation" do
              let(:payload) do
                update_payload_metadata("dependencies", {"chef-client" => "> 2.0.0", "apt" => dep})
              end

              let(:error_message) { "Invalid value '#{dep}' for metadata.dependencies" }

              include_examples("invalid_cookbook_artifact")
            end
          end

          [
            'cats::sleep',
            'here(:kitty, :time_to_eat)',
            'service[snuggle]',
            '',
            1,
            true,
            ['cats', 'sleep', 'here'],

            { 'cats::sleep'                => '0.0.1',
              'here(:kitty, :time_to_eat)' => '0.0.1',
              'service[snuggle]'           => '0.0.1'  }
          ].each do |valid_provides|

            # http://docs.chef.io/config_rb_metadata.html#provides
            context "with metadata.providing set to valid value #{valid_provides}" do

              let(:payload) { update_payload_metadata("providing", valid_provides) }

              include_examples("valid_cookbook_artifact")

            end
          end
        end # context checking metadata sections

        context 'with invalid cookbook artifact identifier in url' do

          let(:cookbook_identifier) { "foo@bar" }
          let(:payload){ new_cookbook_artifact(cookbook_name, cookbook_identifier)}

          it "should respond with an error" do
            put(request_url, requestor, :payload => payload) do |response|
              expect(response.code).to eq(400)
              expect(parse(response)).to eq({"error" => ["Field 'identifier' invalid"]})
            end
          end # it invalid version in URL is a 400
        end # with invalid version in url

        context "when the cookbook name is invalid" do

          let(:cookbook_name) { "first@second" }
          let(:payload){ new_cookbook_artifact(cookbook_name, default_cookbook_id)}

          it "responds with a 400" do
            put(request_url, requestor, :payload => payload) do |response|
              expect(response).to look_like({
                :status => 400,
                :body => {
                  "error" => ["Field 'name' invalid"]
                }
              })
            end
          end
        end

        context "when the identifier in the URL doesn't match the payload" do
          let(:payload) do
            new_cookbook_artifact(cookbook_name, "ffffffffffffffffffffffffffffffffffffffff")
          end

          let(:request_url) { api_url("/#{cookbook_url_base}/#{cookbook_name}/#{default_cookbook_id}") }

          it "responds with a 400" do
            put(request_url, requestor, :payload => payload) do |response|
              error = "Field 'identifier' invalid : 1111111111111111111111111111111111111111 does not match ffffffffffffffffffffffffffffffffffffffff"
              response.should look_like({
                :status => 400,
                :body => {
                  "error" => [error]
                }
              })
            end
          end # it mismatched metadata.cookbook_version is a 400
        end

        context "when the cookbook name in the URL doesn't match the payload" do
          let(:payload) { new_cookbook_artifact("foobar", cookbook_version) }
          let(:request_url) { api_url("/#{cookbook_url_base}/#{cookbook_name}/#{cookbook_version}") }

          it "mismatched cookbook_name is a 400" do
            put(request_url, requestor, :payload => payload) do |response|
              error = "Field 'name' invalid : cookbook_name does not match foobar"
              expect(response).to look_like({
                :status => 400,
                :body => {
                  "error" => [error]
                }
              })
            end
          end
        end

        context "when uploading a cookbook artifact with a missing checksum" do
          after(:each) do
            delete_cookbook_artifact(admin_user, cookbook_name, cookbook_identifier)
          end

          let(:payload) do
            new_cookbook_artifact(cookbook_name, default_cookbook_id).tap do |p|
              p["recipes"] = [
                {
                  "name" => "default.rb",
                  "path" => "recipes/default.rb",
                  "checksum" => "8288b67da0793b5abec709d6226e6b73",
                  "specificity" => "default"
                }
              ]
            end
          end

          it "specifying file not in sandbox is a 400" do
            put(request_url, requestor, :payload => payload) do |response|
              error = "Manifest has a checksum that hasn't been uploaded."
              expect(response).to look_like({
                :status => 400,
                :body => {
                  "error" => [error]
                }
              })
            end
          end # it specifying file not in sandbox is a 400
        end # context sandbox checks
      end # context creating broken cookbook_artifacts to test validation and defaults

      context "creating good cookbook_artifacts to test defaults" do
        let(:cookbook_name) { "cookbook_name" }
        let(:cookbook_version) { "1.2.3" }

        let(:cookbook_identifier) { default_cookbook_id }

        let(:description) { "my cookbook" }
        let(:long_description) { "this is a great cookbook" }
        let(:maintainer) { "This is my name" }
        let(:maintainer_email) { "cookbook_author@example.com" }
        let(:license) { "MPL" }

        let (:opts) {
          {
            :version => cookbook_version,
            :description => description,
            :long_description => long_description,
            :maintainer => maintainer,
            :maintainer_email => maintainer_email,
            :license => license
          }
        }

        after :each do
          delete_cookbook_artifact(admin_user, cookbook_name, cookbook_identifier)
        end

        respects_maximum_payload_size

        let(:payload) do
          new_cookbook_artifact(cookbook_name, cookbook_identifier, opts)
        end

        let(:expected_get_response_data) do
          payload.dup.tap do |artifact|
            artifact.delete("json_class")
          end
        end

        it "allows override of defaults" do
          put(request_url, requestor, :payload => payload) do |response|
            expect(response.code).to eq(201)
          end
          get(request_url, requestor) do |response|
            expect(response).to look_like({
              :status => 200,
              :body => expected_get_response_data
            })
          end
        end # it allows override of defaults
      end # context creating good gookbooks to test defaults
    end # context PUT /cookbook_artifacts/<name>/<version> [create]

    context "PUT multiple cookbook_artifacts" do
      let(:cookbook_name) { "multiple_versions" }
      let(:cookbook_id_1) { "1111111111111111111111111111111111111111" }
      let(:cookbook_id_2) { "2222222222222222222222222222222222222222" }

      let(:cookbook_1_payload) do
        new_cookbook_artifact(cookbook_name, cookbook_id_1, {})
      end

      let(:cookbook_1_get_response) do
        cookbook_1_payload.dup.tap { |c| c.delete("json_class") }
      end

      let(:cookbook_2_payload) do
        new_cookbook_artifact(cookbook_name, cookbook_id_2, {})
      end

      let(:cookbook_2_get_response) do
        cookbook_2_payload.dup.tap { |c| c.delete("json_class") }
      end

      after :each do
        [cookbook_id_1, cookbook_id_2].each do |v|
          delete(api_url("/#{cookbook_url_base}/#{cookbook_name}/#{v}"), admin_user)
        end
      end

      it "allows us to create 2 revisions of the same cookbook" do
        put(api_url("/#{cookbook_url_base}/#{cookbook_name}/#{cookbook_id_1}"),
            admin_user,
            :payload => cookbook_1_payload) do |response|
              expect(response.code).to eq(201)
            end

            put(api_url("/#{cookbook_url_base}/#{cookbook_name}/#{cookbook_id_2}"),
                admin_user,
                :payload => cookbook_2_payload) do |response|
                  expect(response.code).to eq(201)
                end

                get(api_url("/#{cookbook_url_base}/#{cookbook_name}/#{cookbook_id_1}"),
                    admin_user) do |response|
                  response.should look_like({
                    :status => 200,
                    :body => cookbook_1_get_response
                  })
                end

                get(api_url("/#{cookbook_url_base}/#{cookbook_name}/#{cookbook_id_2}"),
                    admin_user) do |response|
                  response.should look_like({
                    :status => 200,
                    :body => cookbook_2_get_response
                  })
                end
      end # it allows us to create 2 versions of the same cookbook
    end # context PUT multiple cookbook_artifacts
  end

  describe "API v0" do
    it_behaves_like "creates cookbook artifacts", 0
  end

  describe "API v2" do
    it_behaves_like "creates cookbook artifacts", 2
  end

end # describe cookbook_artifacts API endpoint
