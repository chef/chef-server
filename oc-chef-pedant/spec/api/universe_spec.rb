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
describe 'Universe API Endpoint', :universe do
  let(:cookbook_url_base) { "cookbooks" }
  include Pedant::RSpec::CookbookUtil

  let(:admin_requestor){ admin_user }

  let(:requestor){ admin_requestor }

  let(:universe_url){ api_url("/universe") }

  let(:universe){ get(universe_url, requestor) }

  let(:cb_one_name){ "foo" }
  let(:cb_one_version){ "1.2.3" }
  let(:cb_one_opts){{ dependencies: { "bar" => ">= 1.1.1" }}}

  let(:cb_two_name){ "bar" }
  let(:cb_two_version){ "1.2.3" }

  context "with no cookbooks" do
    it "returns an empty list" do
      expect(universe.code).to eq(200)
      response_obj = parse(universe.body)
      expect(response_obj).to be_a_kind_of(Hash)
      expect(response_obj).to eq({})
    end
  end

  context "with some cookbooks" do
    let(:expected_response) do
      {
        "foo" => {
          "1.2.3" => {
            "dependencies" => { "bar" => ">= 1.1.1" },
            "location_path" => api_url("cookbooks/foo/1.2.3"),
            "location_type" => "chef_server"
          }
        },
        "bar" => {
          "1.2.3" => {
            "dependencies" => {},
            "location_path" => api_url("cookbooks/bar/1.2.3"),
            "location_type" => "chef_server"
          }
        }
      }
    end
    before do
      make_cookbook(admin_user, cb_one_name, cb_one_version, cb_one_opts)
      make_cookbook(admin_user, cb_two_name, cb_two_version)
    end

    after do
      delete_cookbook(admin_user, cb_one_name, cb_one_version)
      delete_cookbook(admin_user, cb_two_name, cb_two_version)
    end

    it "is successful" do
      expect(universe.code).to eq(200)
    end

    it "returns the expected results" do
      response_obj = parse(universe.body)
      expect(response_obj).to be_a_kind_of(Hash)
      expect(response_obj).to strictly_match(expected_response)
    end
  end
end
