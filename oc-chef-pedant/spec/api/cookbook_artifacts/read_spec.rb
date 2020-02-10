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

require 'pedant/rspec/cookbook_util'

# FIXME  We don't test GET /cookbook_artifacts/NAME/VERSION when we have
# any files in the segments.  Thus we're not checking the creation
# of the S3 URLs that should be returned for all the files in the
# cookbook

describe "Cookbook Artifacts API endpoint", :cookbook_artifacts, :cookbook_artifacts_read do

  shared_examples "reads cookbook artifacts" do |api_version|
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

    include Pedant::RSpec::CookbookUtil

    def cookbook_artifact_version_url(name, identifier)
      api_url("/#{cookbook_url_base}/#{name}/#{identifier}")
    end

    context "GET /cookbook_artifacts" do
      let(:request_url){api_url("/#{cookbook_url_base}/")}
      let(:requestor) { admin_user }

      context "with no cookbook artifacts on the server", :smoke do
        it "responds with 200" do
          get(request_url, requestor) do |response|
            expect(response.code).to eq(200)
            expect(parse(response)).to eq({})
          end
        end
      end

      context "with existing cookbook_artifacts and multiple versions" do

        let(:request_url) { api_url("/#{cookbook_url_base}") }

        let(:fetched_cookbook_artifacts) { cookbook_collection }

        let(:default_version) { "1.0.0" }

        let(:cookbook_name) { "cookbook_name" }
        let(:cookbook_name2) { "cookbook_name2" }
        let(:identifier_1) { "1111111111111111111111111111111111111111" }
        let(:identifier_2) { "2222222222222222222222222222222222222222" }
        let(:identifier_3) { "3333333333333333333333333333333333333333" }

        let(:cba_1_url) { request_url + "/#{cookbook_name}/#{identifier_1}" }
        let(:cba_2_url) { request_url + "/#{cookbook_name}/#{identifier_2}" }
        let(:cba_3_url) { request_url + "/#{cookbook_name2}/#{identifier_3}" }

        let(:cba_1) { new_cookbook_artifact(cookbook_name, identifier_1) }
        let(:cba_2) { new_cookbook_artifact(cookbook_name, identifier_2) }
        let(:cba_3) { new_cookbook_artifact(cookbook_name2, identifier_3) }

        before(:each) do
          r1 = put(cba_1_url, admin_user, payload: cba_1)
          expect(r1.code).to eq(201)

          r2 = put(cba_2_url, admin_user, payload: cba_2)
          expect(r2.code).to eq(201)

          r3 = put(cba_3_url, admin_user, payload: cba_3)
          expect(r3.code).to eq(201)
        end

        after(:each) do
          r1 = delete(cba_1_url, admin_user)
          expect(r1.code).to eq(200)

          r2 = delete(cba_2_url, admin_user)
          expect(r2.code).to eq(200)

          r3 = delete(cba_3_url, admin_user)
          expect(r3.code).to eq(200)
        end

        let(:expected_cookbook_artifact_collection) do
          {
            cookbook_name => {
              "url" => cookbook_url(cookbook_name),
              "versions" => [
                { "identifier" => identifier_1,
                  "url" => cookbook_artifact_version_url(cookbook_name, identifier_1) },
              { "identifier" => identifier_2,
                "url" => cookbook_artifact_version_url(cookbook_name, identifier_2) }
              ]},
              cookbook_name2 => {
                "url" => cookbook_url(cookbook_name2),
                "versions" => [
                  { "identifier" => identifier_3,
                    "url" => cookbook_artifact_version_url(cookbook_name2, identifier_3) }]}
          }
        end

        it 'should respond with a cookbook collection containing all versions of each cookbook' do
          list_response = get(request_url, requestor)
          expect(list_response.code).to eq(200)
          expect(parse(list_response)).to strictly_match(expected_cookbook_artifact_collection)
        end

      end

    end # context GET /cookbook_artifacts

    context "GET /cookbook_artifacts/<name>/<version>" do


      let(:cookbook_name) { "the_cookbook_name" }
      let(:default_version) { "1.2.3" }
      let(:cookbook_identifier) { "1111111111111111111111111111111111111111" }

      let(:request_url)    { cookbook_artifact_version_url(cookbook_name, cookbook_identifier) }

      let(:recipe_name) { "test_recipe" }
      let(:recipe_content) { "hello-#{unique_suffix}" }

      let(:recipe_spec) do
        {
          :name => recipe_name,
          :content => recipe_content
        }
      end

      before(:each) do
        make_cookbook_artifact_with_recipes(cookbook_name, cookbook_identifier, [recipe_spec])
      end

      after(:each)  { delete_cookbook_artifact(admin_user, cookbook_name, cookbook_identifier) }

      shared_examples_for "successful_cookbook_fetch" do

        let(:expected_cookbook_artifact_data) do
          new_cookbook_artifact(cookbook_name, cookbook_identifier).tap do |cba|
            cba.delete("json_class")

            # checksum and url are also present in the real data but they're not
            # stable so we remove them before comparing
            target = platform.server_api_version >= 2 ? "all_files" : "recipes"
            rn = platform.server_api_version >= 2 ? "recipes/#{recipe_name}.rb" : "#{recipe_name}.rb"
            cba[target] = [
              {
                "name" => rn,
                "path" => "recipes/#{recipe_name}.rb",
                "specificity" => "default"
              }
            ]
            cba["metadata"]["providing"] = { "#{cookbook_name}::#{recipe_name}"=>">= 0.0.0" }
            cba["metadata"]["recipes"] = { "#{cookbook_name}::#{recipe_name}"=>"" }
          end
        end

        it "returns a 200 response" do
          get_response = get(request_url, requestor)
          expect(get_response.code).to eq(200)
          response_data = parse(get_response)
          recipes = extract_segment(response_data, "recipes")
          expect(recipes).to be_a_kind_of(Array)
          expect(recipes.size).to eq(1)
          expect(recipes.first.keys).to match_array(%w[ name path checksum specificity url ])

          # URL and checksum are not predictable.
          target = platform.server_api_version >= 2 ? "all_files" : "recipes"
          response_data[target].first.delete("url")
          response_data[target].first.delete("checksum")

          expect_matching_cookbook_artifact(response_data, expected_cookbook_artifact_data)
        end

        # These tests verify that the pre-signed URL that comes back
        # within cookbook_version responses is usable. Validating both
        # the URL generation, but also the use of bookshelf via the
        # load balancer.
        it "returns valid file URLs" do
          cookbook_artifact_data = parse(get(request_url, requestor))
          recipe_url = extract_segment(cookbook_artifact_data, "recipes").first["url"]

          uri = URI.parse(recipe_url)
          http = Net::HTTP.new(uri.hostname, uri.port)
          if uri.scheme == "https"
            http.use_ssl = true
            http.ssl_version = Pedant::Config.ssl_version
            http.verify_mode = OpenSSL::SSL::VERIFY_NONE
            http.cert        = Pedant::Config.ssl_client_cert if Pedant::Config.ssl_client_cert
            http.key         = Pedant::Config.ssl_client_key  if Pedant::Config.ssl_client_key
            http.ca_file     = Pedant::Config.ssl_ca_file     if Pedant::Config.ssl_ca_file
          end

          # Chef::ServerAPI always sets the Host
          # header to HOSTNAME:PORT. We do the same here to avoid sigv4
          # signing issues.
          response = http.get(uri.request_uri, {"Host" => "#{uri.hostname}:#{uri.port}"})
          response.body.should == recipe_content
        end

      end # as a normal user

      context 'as a normal user' do
        let(:requestor) { normal_user }

        include_examples("successful_cookbook_fetch")
      end

      context 'as an admin user' do
        let(:requestor) { admin_user }

        include_examples("successful_cookbook_fetch")
      end # as an admin user

      context 'as an user outside of the organization', :authorization do
        let(:request_method) { :GET }
        let(:expected_response) { unauthorized_access_credential_response }
        let(:requestor) { outside_user }

        should_respond_with 403
      end # as an outside user

      context 'with invalid user', :authentication do
        let(:request_method) { :GET }
        let(:expected_response) { invalid_credential_exact_response }
        let(:requestor) { invalid_user }

        should_respond_with 401
      end
    end # context GET /cookbook_artifacts/<name>/<version>
  end

  describe "API v0" do
    it_behaves_like "reads cookbook artifacts", 0
  end

  describe "API v2" do
    it_behaves_like "reads cookbook artifacts", 2
  end

end # describe cookbook_artifacts API endpoint
