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

describe "Cookbook Artifacts API endpoint", :cookbook_artifacts, :cookbook_artifacts_create do

  TEST_NAME = "cookbook_artifacts_create"
  TEST_IDENTIFIER = "1111111111111111111111111111111111111111"
  TEST_COOKBOOK = "/cookbook_artifacts/#{TEST_NAME}/#{TEST_IDENTIFIER}"

  include Pedant::RSpec::CookbookUtil

  context "PUT #{TEST_COOKBOOK} [create]" do

    include Pedant::RSpec::Validations::Create

    let(:cookbook_artifact_to_create){ new_cookbook_artifact(TEST_COOKBOOK) }

    context 'with a basic cookbook', :smoke do
      let(:expected_get_response_data) do
        {
          "name"=>TEST_NAME,
          "identifier"=>TEST_IDENTIFIER,
          "version"=>"1.0.0",
          "chef_type"=>"cookbook_version",
          "frozen?"=>false,
          "recipes"=>[],
          "metadata"=> {
            "version"=>"1.0.0",
            "name"=>"pedant_basic",
            "maintainer"=>"Your Name",
            "maintainer_email"=>"youremail@example.com",
            "description"=>"A fabulous new cookbook",
            "long_description"=>"",
            "license"=>"Apache v2.0",
            "dependencies"=>{},
            "attributes"=>{},
            "recipes"=>{}
          }
        }
      end

      it "creates a basic cookbook_artifact" do
        # create it:
        expect(
          put(TEST_COOKBOOK, payload: cookbook_artifact_to_create)
        ).to respond_with(201)

        # list:
        list_response = get("/cookbook_artifacts/#{TEST_NAME}", requestor)

        list_data = parse(list_response)
        expect(list_data).to have_key("pedant_basic")
        expect(list_data["pedant_basic"]["versions"].size).to eq(1)
        expect(list_data["pedant_basic"]["versions"].first["identifier"]).to eq(TEST_IDENTIFIER)

        # fetch it:
        expect(
          get(TEST_COOKBOOK, requestor)
        ).to respond_with(200, body_exact: expected_get_response_data)
      end
    end

    def expect_cookbook_artifact_to_be_created(without: [], **fields)
      payload = new_cookbook_artifact(cookbook_path, fields)
      without.each do |value|
      expect(put(cookbook_path, payload: payload)).to respond_with(201)
      expect(get(cookbook_path)).to respond_with(200, body: Hash[fields.map { |k,v| [k.to_s,v] }])
    end
    def self.cookbook_artifact_create_succeeds(description=nil, without: [], **fields)
      description ||= begin
        description = []
        description << "with #{fields.map { |k,v| "#{k}=#{v.inspect}"}.join(", ")}" unless fields.empty?
        description << "without #{without.join(", ")}" unless without.empty?
        description.join(" and ")
      end
      description = " #{description}" if !description.empty?

      it "Creating a cookbook artifact#{description} succeeds" do
        payload = new_cookbook_artifact(TEST_COOKBOOK, fields)
        without.each { |field| payload.delete(field.to_s) }
        expect(put(TEST_COOKBOOK, payload: payload)).to respond_with(201)
        expected_body = Hash[fields.map { |k,v| [k.to_s,v] }.reject { |k,v| k == "json_class" }]
        expect(get(TEST_COOKBOOK)).to respond_with(200, body: expected_body)
      end
    end
    def self.cookbook_artifact_create_fails(code, error_message, *tags, without: [], **fields)
      description ||= begin
        description = []
        description << "with #{fields.map { |k,v| "#{k}=#{v.inspect}"}.join(", ")}" unless fields.empty?
        description << "without #{without.join(", ")}" unless without.empty?
        description.join(" and ")
      end
      description = " #{description}" if description
      tags << :validation if code == 400
      tags << :authentication if code == 401
      tags << :authorization if code == 403
      tags << :validation if code == 413

      it "Creating a cookbook artifact#{description} fails with #{code}: #{error_message}" do
        payload = new_cookbook_artifact(TEST_COOKBOOK, fields)
        without.each { |field| payload.delete(field.to_s) }
        expect(put(TEST_COOKBOOK, payload: payload)).to respond_with(code, error_message: error_message)
        expect(get(TEST_COOKBOOK)).to respond_with(404)
      end
    end

    # Start using the new validation macros
    context "when validating fields" do
      cookbook_artifact_create_succeeds "with all valid values"
      context "version" do
        cookbook_artifact_create_succeeds "with a version exactly 4 bytes", version: "1.2.2147483647"
        cookbook_artifact_create_succeeds "with a version greater than 4 bytes", version: "1.2.2147483669"
        cookbook_artifact_create_succeeds "with a prerelease version", version: "1.2.3.beta.5"
      end
      context "Field json_class" do
        # cookbook_artifact_create_succeeds without: :json_class
        # cookbook_artifact_create_fails 400, json_class: "blah"
      end
    end

    context "creating broken cookbook_artifacts to test validation and defaults", :validation do

      let(:base_payload) do
        new_cookbook_artifact(TEST_COOKBOOK, version: cookbook_version).tap do |ca|
          ca.delete("json_class")
        end
      end

      def update_payload(key, value)
        base_payload.dup.tap do |payload|
          if value == :delete
            payload.delete(key)
          else
            payload[key] = value
          end
        end
      end

      shared_examples_for "valid_cookbook_artifact" do

        it "create returns 201" do
          #create it
          expect(
            put(TEST_COOKBOOK, payload: payload)
          ).to respond_with(201)

          # verify it looks correct
          expect(
            get(TEST_COOKBOOK)
          ).to respond_with(200, body: payload)
        end
      end

      shared_examples_for "invalid_cookbook_artifact" do
        it "create returns 400" do
          # create should respond with error
          expect(
            put(TEST_COOKBOOK, payload: payload)
          ).to respond_with(400, error_message: error_message)

          # double check it did't get created
          expect(
            get(TEST_COOKBOOK)
          ).to respond_with(404)
        end
      end

      context "basic tests" do
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
              if value == :delete
                md.delete(key)
              else
                md[key] = value
              end
            end
          end
        end

        let(:malformed_constraint) { "s395dss@#" }

        %w{platforms dependencies recommendations suggestions conflicting replacing}.each do |section|

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

          # http://docs.opscode.com/config_rb_metadata.html#provides
          context "with metadata.providing set to valid value #{valid_provides}" do

            let(:payload) { update_payload_metadata("providing", valid_provides) }

            include_examples("valid_cookbook_artifact")

          end
        end
      end # context checking metadata sections

      context 'with invalid cookbook artifact identifier in url' do

        let(:payload){ new_cookbook_artifact("/cookbook_artifacts/#{TEST_NAME}/invalid@identifier")}

        it "should respond with an error" do
          expect(
            put(TEST_COOKBOOK, payload: payload)
          ).to respond_with(400, { "error" => ["Field 'identifier' invalid"] })
        end # it invalid version in URL is a 400
      end # with invalid version in url

      context "when the cookbook name is invalid" do

        let(:payload){ new_cookbook_artifact("/cookbook_artifacts/invalid@identifier/#{TEST_IDENTIFIER}")}

        it "responds with a 400" do
          expect(
            put(TEST_COOKBOOK, payload: payload)
          ).to respond_with(400, body: { "error" => ["Field 'name' invalid"] })
        end
      end

      context "when the identifier in the URL doesn't match the payload" do
        let(:payload) do
          new_cookbook_artifact("/cookbook_artifacts/#{TEST_NAME}/ffffffffffffffffffffffffffffffffffffffff")
        end

        it "responds with a 400" do
          expect(
            put(TEST_COOKBOOK, payload: payload)
          ).to respond_with(400, error_message: "Field 'identifier' invalid : #{TEST_IDENTIFIER} does not match ffffffffffffffffffffffffffffffffffffffff")
        end # it mismatched metadata.cookbook_version is a 400
      end

      context "when the cookbook name in the URL doesn't match the payload" do
        let(:payload) { new_cookbook_artifact("/cookbook_artifacts/foobar/#{cookbook_version}") }

        it "mismatched cookbook name is a 400" do
          expect(
            put(TEST_COOKBOOK, payload: payload)
          ).to respond_with(400, error_message: "Field 'name' invalid : #{TEST_NAME} does not match foobar")
        end
      end

      context "when uploading a cookbook artifact with a missing checksum" do
        let(:payload) do
          new_cookbook_artifact("/cookbook_artifacts/#{TEST_NAME}/#{default_cookbook_id}").tap do |p|
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
          expect(
            put(TEST_COOKBOOK, payload: payload)
          ).to respond_with(400, error_message: "Manifest has a checksum that hasn't been uploaded.")
        end # it specifying file not in sandbox is a 400
      end # context sandbox checks
    end # context creating broken cookbook_artifacts to test validation and defaults

    it "Disallows excessive requests (greater than #{MAXIMUM_PAYLOAD_SIZE} bytes)" do
      payload = new_cookbook_artifact(TEST_COOKBOOK, long_description: "PADME"))
      payload = pad_payload_to(payload, MAXIMUM_PAYLOAD_SIZE+1)
      expect(
        put(TEST_COOKBOOK, payload: payload)
      ).to respond_with(413)
      expect(
        get(TEST_COOKBOOK)
      ).to respond_with(404)
    end

    it "Allows maximally-sized requests (equal to #{MAXIMUM_PAYLOAD_SIZE} bytes)" do
      valid_payload = new_cookbook_artifact(TEST_COOKBOOK, long_description: "PADME"))
      payload = pad_payload_to(payload, MAXIMUM_PAYLOAD_SIZE+1)
      expect(
        put(TEST_COOKBOOK, payload: payload)
      ).to respond_with(201, body_exact: payload)
      expect(
        get(TEST_COOKBOOK)
      ).to respond_with(200, body: payload.reject { |k,v| k == "json_class"})
    end

    it "Allows PUT to override defaults" do
      payload = new_cookbook_artifact(TEST_COOKBOOK,
        version: "1.2.3",
        description: "my cookbook",
        long_description: "this is a great cookbook",
        maintainer: "This is my name",
        maintainer_email: "cookbook_author@example.com",
        license: "MPL"
      )

      expect(
        put(TEST_COOKBOOK, payload: payload)
      ).to respond_with(201)

      expect(
        get(TEST_COOKBOOK)
      ).to respond_with(200, body: payload.reject { |k,v| k == "json_class"})
    end # it allows override of defaults
  end # context PUT /cookbook_artifacts/<name>/<version> [create]

  context "PUT multiple cookbook_artifacts" do
    let(:cookbook_id_1) { TEST_IDENTIFIER }
    let(:cookbook_id_2) { "2222222222222222222222222222222222222222" }

    let(:cookbook_1_payload) do
      new_cookbook_artifact("/cookbook_artifacts/#{TEST_NAME}/#{cookbook_id_1}", {})
    end

    let(:cookbook_1_get_response) do
      cookbook_1_payload.dup.tap { |c| c.delete("json_class") }
    end

    let(:cookbook_2_payload) do
      new_cookbook_artifact("/cookbook_artifacts/#{TEST_NAME}/#{cookbook_id_2}", {})
    end

    let(:cookbook_2_get_response) do
      cookbook_2_payload.dup.tap { |c| c.delete("json_class") }
    end

    it "allows us to create 2 revisions of the same cookbook" do
      expect(
        put("/cookbook_artifacts/#{TEST_NAME}/#{cookbook_id_1}", payload: cookbook_1_payload)
      ).to respond_with(201)

      expect(
        put("/cookbook_artifacts/#{TEST_NAME}/#{cookbook_id_2}", payload: cookbook_2_payload)
      ).to respond_with(201)

      expect(
        get("/cookbook_artifacts/#{TEST_NAME}/#{cookbook_id_1}")
      ).to respond_with(200, body: cookbook_1_get_response)

      expect(
        get("/cookbook_artifacts/#{TEST_NAME}/#{cookbook_id_2}")
      ).to respond_with(200, body: cookbook_2_get_response)
    end # it allows us to create 2 versions of the same cookbook
  end # context PUT multiple cookbook_artifacts
end # describe cookbook_artifacts API endpoint
