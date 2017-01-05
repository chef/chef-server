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

describe "Cookbooks API endpoint", :cookbooks, :cookbooks_delete do

  let(:cookbook_url_base) { "cookbooks" }

  include Pedant::RSpec::CookbookUtil

  context "DELETE /cookbooks/<name>/<version>" do
    let(:request_method) { :DELETE }
    let(:request_url)    { named_cookbook_url }
    let(:requestor)      { admin_user }

    let(:fetched_cookbook) { original_cookbook }
    let(:original_cookbook) { new_cookbook(cookbook_name, cookbook_version) }

    context "for non-existent cookbooks" do
      let(:expected_response) { cookbook_version_not_found_exact_response }

      let(:cookbook_name)    { "non_existent" }
      let(:cookbook_version) { "1.2.3" }

      should_respond_with 404

      context 'with bad version', :validation do
        let(:expected_response) { invalid_cookbook_version_exact_response }
        let(:cookbook_version) { "1.2.3.4" }
        should_respond_with 400
      end # with bad version
    end # context for non-existent cookbooks

    context "for existing cookbooks" do
      let(:cookbook_name) { "cookbook-to-be-deleted" }
      let(:cookbook_version) { "1.2.3" }

      context "when deleting non-existent version of an existing cookbook" do
        let(:expected_response) { cookbook_version_not_found_exact_response }
        let(:cookbook_version_not_found_error_message) { ["Cannot find a cookbook named #{cookbook_name} with version #{non_existing_version}"] }
        let(:non_existing_version) { "99.99.99" }
        let(:non_existing_version_url) { api_url("/#{cookbook_url_base}/#{cookbook_name}/#{non_existing_version}") }
        let(:fetched_cookbook) { original_cookbook }

        before(:each) { make_cookbook(admin_user, cookbook_name, cookbook_version) }
        after(:each) { delete_cookbook(admin_user, cookbook_name, cookbook_version) }

        it "should respond with 404 (\"Not Found\") and not delete existing versions" do
          delete(non_existing_version_url, admin_user) do |response|
            response.should look_like expected_response
          end

          should_not_be_deleted
        end
      end # it doesn't delete the wrong version of an existing cookbook

      context "when deleting existent version of an existing cookbook", :smoke do
        let(:recipe_name) { "test_recipe" }
        let(:recipe_content) { "hello-#{unique_suffix}-#{rand(1000)}-#{rand(1000)}" }
        let(:recipe_spec) do
            {
              :name => recipe_name,
              :content => recipe_content
            }
        end

        let(:cookbooks) do
          {
            cookbook_name => {
              cookbook_version => [recipe_spec]
            }
          }
        end

        let(:fetched_cookbook) { original_cookbook }
        let(:original_cookbook) { new_cookbook(cookbook_name, cookbook_version) }

        before(:each) { setup_cookbooks(cookbooks) }
        after(:each)  { remove_cookbooks(cookbooks) }

        it "should cleanup unused checksum data in s3/bookshelf" do
          verify_checksum_cleanup(:recipes) do
            response.should look_like(:status => 200)
          end
        end

      end # context when deleting existent version...
    end # context for existing cookbooks

    context "with permissions for" do
      let(:cookbook_name) {"delete-cookbook"}
      let(:cookbook_version) { "0.0.1" }
      let(:not_found_msg) { ["Cannot find a cookbook named delete-cookbook with version 0.0.1"] }

      before(:each) { make_cookbook(admin_user, cookbook_name, cookbook_version) }
      after(:each) { delete_cookbook(admin_user, cookbook_name, cookbook_version) }

      context 'as admin user' do
        let(:expected_response) { delete_cookbook_success_response }

        it "should respond with 200 (\"OK\") and be deleted" do
          should look_like expected_response
          should_be_deleted
        end # it admin user returns 200
      end # as admin user

      context 'as normal user', :authorization do
        let(:expected_response) { delete_cookbook_success_response }

        let(:requestor) { normal_user }
        it "should respond with 200 (\"OK\") and be deleted" do
          should look_like expected_response
          should_be_deleted
        end # it admin user returns 200
      end # with normal user

      context 'as a user outside of the organization', :authorization do
        let(:expected_response) { unauthorized_access_credential_response }
        let(:requestor) { outside_user }

        it "should respond with 403 (\"Forbidden\") and does not delete cookbook" do
          response.should look_like expected_response
          should_not_be_deleted
        end
      end # it outside user returns 403

      context 'with invalid user', :authorization do
        let(:expected_response) { invalid_credential_exact_response }
        let(:requestor) { invalid_user }

        it "should respond with 401 (\"Unauthorized\") and does not delete cookbook" do
          response.should look_like expected_response
          should_not_be_deleted
        end # responds with 401
      end # with invalid user

    end # context with permissions for
  end # context DELETE /cookbooks/<name>/<version>
end
