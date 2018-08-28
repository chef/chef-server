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

describe "Cookbook Versions API endpoint, GET", :cookbooks, :cookbooks_version do

  shared_examples "versions cookbooks" do |api_version|
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
    let(:request_url)    { named_cookbook_url }
    let(:requestor)      { admin_user }

    let(:non_existent_cookbook){ "fakecookbook" }
    let(:fake_version){ "1.0.0" }
    let(:latest_cookbook_version_url) { api_url("/#{cookbook_url_base}/#{cookbook_name}/_latest") }

    let(:fetch_cookbook_version_success_response) do
      {
        :status => 200,
        :body => retrieved_cookbook(cookbook_name, cookbook_version)
      }
    end
    let(:cookbook_version_not_found_exact_response) do
      {
        :status => 404,
        :body_exact => { "error" => ["Cannot find a cookbook named #{cookbook_name} with version #{cookbook_version}"] }
      }
    end

    context "with no cookbooks on the server" do
      let(:expected_response) { cookbook_version_not_found_exact_response }
      let(:cookbook_name) { non_existent_cookbook }
      let(:cookbook_version) { fake_version }

      should_respond_with 404
    end # with no cookbooks on the server

    context "with cookbooks on the server" do

      let(:cookbook_name) { existing_cookbook_name }
      let(:cookbook_version) { existing_cookbook_version }
      let(:existing_cookbook_name){ "the_art_of_french_cooking" }
      let(:existing_cookbook_version){ "1.0.0" }
      let(:latest){ "1.0.1" }

      before :each do
        make_cookbook(admin_user, existing_cookbook_name, existing_cookbook_version)
        # Erchef doesn't handle multipe versions yet
        make_cookbook(admin_user, existing_cookbook_name, latest)
      end

      after :each do
        delete_cookbook(admin_user, existing_cookbook_name, existing_cookbook_version)
        delete_cookbook(admin_user, existing_cookbook_name, latest)
      end

      context 'when fetching existing version of cookbook' do
        let(:expected_response) { fetch_cookbook_version_success_response }

        should_respond_with 200, 'and the cookbook version'
      end

      context 'when fetching non-existant version of cookbook' do
        let(:expected_response) { cookbook_version_not_found_exact_response }
        let(:cookbook_version) { '6.6.6' }

        should_respond_with 404
      end

      context 'as a non-admin user' do
        let(:expected_response) { fetch_cookbook_version_success_response }
        let(:requestor) { normal_user }

        should_respond_with 200, 'and the cookbook version'
      end

      context 'as a user outside the organization', :authorization do
        let(:expected_response) { unauthorized_access_credential_response }
        let(:requestor) { outside_user }

        should_respond_with 403
      end

      context "when requesting the 'latest' Cookbook version", :smoke do
        let(:expected_response) { fetch_cookbook_version_success_response }
        let(:request_url) { latest_cookbook_version_url }
        let(:cookbook_version) { latest }

        should_respond_with 200, 'and the latest cookbook version'
      end # when requesting the 'latest' cookbook version

      context "when requesting the 'latest' version of a non-existent cookbook" do
        let(:expected_response) { cookbook_version_not_found_exact_response }
        let(:cookbook_name) { non_existent_cookbook }
        let(:cookbook_version) { '_latest' }

        should_respond_with 404
      end # when requesting the 'latest' version of a non-existent cookbook
    end # with existing cookbook
  end

  describe "API v0" do
    it_behaves_like "versions cookbooks", 0
  end

  describe "API v2" do
    it_behaves_like "versions cookbooks", 2
  end
end
