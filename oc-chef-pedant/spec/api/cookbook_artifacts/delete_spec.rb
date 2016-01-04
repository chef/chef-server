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

require 'pedant/rspec/cookbook_util'

describe "Cookbook Artifacts API endpoint", :cookbook_artifacts, :cookbook_artifacts_delete do

  include Pedant::RSpec::CookbookUtil

  TEST_NAME = "cookbook_artifact_delete"
  TEST_IDENTIFIER = "1111111111111111111111111111111111111111"
  TEST_COOKBOOK = "/cookbook_artifacts/#{TEST_NAME}/#{TEST_IDENTIFIER}"

  context "DELETE #{TEST_COOKBOOK}" do

    context "When there are no cookbook artifacts" do
      it "Deleting a cookbook artifact returns 404" do
        expect(
          delete("/cookbook_artifacts/#{TEST_NAME}_nonexistent/#{TEST_IDENTIFIER}")
        ).to respond_with(404, error_message: "not_found")
      end

      it "Deleting a cookbook artifact with an invalid identifier 404s" do
        expect(
          delete("/cookbook_artifacts/#{TEST_NAME}/foo@bar")
        ).to respond_with(404, error_message: "not_found")
      end
    end # context for non-existent cookbooks

    context "when a cookbook artifact #{TEST_COOKBOOK} exists" do
      before { make_cookbook_artifact(TEST_COOKBOOK) }
      let(:original_cookbook) { new_cookbook_artifact(TEST_COOKBOOK) }
      let(:fetched_cookbook) { original_cookbook.dup.tap { |c| c.delete("json_class") } }

      it "Deleting a different version of #{TEST_COOKBOOK} 404s and does not delete anything" do
        expect(
          delete("/cookbook_artifacts/#{TEST_NAME}/ffffffffffffffffffffffffffffffffffffffff")
        ).to respond_with(404, error_message: "not_found")

        expect(
          get(TEST_COOKBOOK)
        ).to respond_with(200)
      end

      it "Deleting the cookbook should delete it and its parent (since it's the last one)" do
        expect(
          delete(TEST_COOKBOOK)
        ).to respond_with(200)

        expect(get(TEST_COOKBOOK)).to respond_with(404)
        expect(get("/cookbook_artifacts/#{TEST_NAME}")).to respond_with(404)
      end
    end

    context "When cookbook artifact #{TEST_COOKBOOK} contains recipes" do
      before do
        make_cookbook_artifact_with_recipes(TEST_COOKBOOK, [{
          name: "test_recipe",
          content: "hello-#{unique_suffix}"
        }])
      end

      it "deleting the cookbook should delete it and clean up unused checksum data in s3/bookshelf" do
        expect(
          delete(TEST_COOKBOOK)
        ).to respond_with(200)

        checksums = get_cookbook_checksums(TEST_COOKBOOK)
        expect(checksums.size).to eq(1)
        verify_checksum_url(checksums.values.first, 404)
      end
    end # context for existing cookbooks

    context "Permissions" do
      context 'as admin user' do

        it "should respond with 200 (\"OK\") and be deleted" do
          expect(
            delete(TEST_COOKBOOK, admin_user)
          ).to respond_with(200, body_exact: fetched_cookbook)

          expect(
            get(TEST_COOKBOOK)
          ).to respond_with(404)# , error_message: "Cannot find a cookbook named delete-cookbook with version 0.0.1"
        end # it admin user returns 200
      end # as admin user

      context 'as normal user', :authorization do
        it "should respond with 200 (\"OK\") and be deleted" do
          expect(
            delete(TEST_COOKBOOK, normal_user)
          ).to respond_with(200, body_exact: fetched_cookbook)

          expect(
            get(TEST_COOKBOOK)
          ).to respond_with(404)
        end # it admin user returns 200
      end # with normal user

      context 'as a user outside of the organization', :authorization do
        it "should respond with 403 (\"Forbidden\") and does not delete cookbook" do
          expect(
            delete(TEST_COOKBOOK, outside_user)
          ).to look_like(unauthorized_access_credential_response)

          expect(
            get(TEST_COOKBOOK)
          ).to look_like(404, error_message: ["not_found"])
        end
      end # it outside user returns 403

      context 'with invalid user', :authorization do
        it "should respond with 401 (\"Unauthorized\") and does not delete cookbook" do
          expect(
            delete(TEST_COOKBOOK, invalid_user)
          ).to look_like(invalid_credential_exact_response)

          expect(
            get(TEST_COOKBOOK)
          ).to look_like(404, error_message: ["not_found"])
        end # responds with 401
      end # with invalid user

    end # context with permissions for
  end # context DELETE /cookbooks/<name>/<version>
end
