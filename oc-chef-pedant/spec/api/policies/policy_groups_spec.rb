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

describe "/policy_groups API endpoint", :policies, :policy_groups do

  def mutate_json(data)
    parsed = parse(data)
    yield parsed
    to_json(parsed)
  end

  def name_of(policy_data)
    parse(policy_data)["name"]
  end

  def rev_id_of(policy_data)
    parse(policy_data)["revision_id"]
  end

  def modified_policy(name: nil, rev_id: nil)
    base_policy = minimum_valid_policy_payload

    if name
      base_policy = mutate_json(base_policy) do |p|
        p["name"] = name
      end
    end

    if rev_id
      base_policy = mutate_json(base_policy) do |p|
        p["revision_id"] = rev_id
      end
    end

    base_policy
  end

  # Just until we rename the requestors
  let(:admin_requestor){ admin_user }

  let(:requestor){admin_requestor}

  let(:policy_groups_url) { api_url("/policy_groups") }

  let(:request_url) { policy_groups_url }

  let(:minimum_valid_policy_payload) do
    <<-PAYLOAD
      {
        "revision_id": "909c26701e291510eacdc6c06d626b9fa5350d25",
        "name": "some_policy_name",
        "run_list": [
          "recipe[policyfile_demo::default]"
        ],
        "cookbook_locks": {
          "policyfile_demo": {
            "identifier": "f04cc40faf628253fe7d9566d66a1733fb1afbe9",
            "version": "1.2.3"
          }
        }
      }
    PAYLOAD
  end

  let(:static_named_policy_url) { api_url("/policy_groups/some_policy_group/policies/some_policy_name") }

  context "when no policies or policy_groups exist on the server" do

    let(:request_payload) { nil }

    let(:request_method) { :GET }

    it "GET /policy_groups returns an empty list" do
      expect(response.code).to eq(200)
      response_obj = parse(response.body)
      expect(response_obj).to be_a_kind_of(Hash)
      # TODO: currently you can't delete groups so we may have a leftover
      # policy_group "some_policy_group" from complete_endpoint_spec.rb
      # When policy_group deletion is implemented we can improve the cleanup
      # and fix this test
      # expect(response_obj).to eq({})
    end

  end

  context "when a policy assigned to a group exists" do

    before do
      put(static_named_policy_url, requestor, payload: minimum_valid_policy_payload)
    end

    after(:each) do
      delete(static_named_policy_url, requestor)
    end

    let(:request_payload) { nil }

    let(:request_method) { :GET }

    let(:expected_response_data) do
      {
        "some_policy_group" => {
          "uri" => api_url("policy_groups/some_policy_group"),
          "policies" => {
            "some_policy_name" => {
              "revision_id" => "909c26701e291510eacdc6c06d626b9fa5350d25"
            }
          }
        }
      }
    end

    it "GET /policy_groups returns a data structure with the groups assigned policy revision" do
      expect(response.code).to eq(200)
      response_obj = parse(response.body)
      expect(response_obj).to be_a_kind_of(Hash)
      expect(response_obj).to eq(expected_response_data)
    end
  end

  context "when a policy group exists but has no assigned policies" do

    let(:request_payload) { nil }

    let(:request_method) { :GET }

    let(:expected_response_data) do
      {
        "some_policy_group" => {
          "uri" => api_url("policy_groups/some_policy_group")
        }
      }
    end
    before do
      put(static_named_policy_url, requestor, payload: minimum_valid_policy_payload)
      delete(static_named_policy_url, requestor)
    end

    it "GET /policy_groups returns a data structure with an empty policy_group" do
      expect(response.code).to eq(200)
      response_obj = parse(response.body)
      expect(response_obj).to be_a_kind_of(Hash)
      expect(response_obj).to eq(expected_response_data)
    end
  end

  context "with multiple policies and groups" do

    def assoc(policy_data, policy_group)
      policy_name = name_of(policy_data)
      url = api_url("policy_groups/#{policy_group}/policies/#{policy_name}")
      {
        url: url,
        policy_name: policy_name,
        policy_group: policy_group,
        policy_data: policy_data,
        rev_id: rev_id_of(policy_data)
      }
    end

    def push(assoc)
      put(assoc[:url], requestor, payload: assoc[:policy_data])
    end

    def rm(assoc)
      delete(assoc[:url], requestor)
    end

    let(:app_policy_1) { modified_policy(name: "appserver", rev_id: "1" * 40) }
    let(:app_policy_2) { modified_policy(name: "appserver", rev_id: "2" * 40) }
    let(:app_policy_3) { modified_policy(name: "appserver", rev_id: "3" * 40) }

    # match numbering to rev_id instead of starting over
    let(:db_policy_6) { modified_policy(name: "db", rev_id: "6" * 40) }
    let(:db_policy_7) { modified_policy(name: "db", rev_id: "7" * 40) }
    let(:db_policy_8) { modified_policy(name: "db", rev_id: "8" * 40) }

    # match numbering to rev_id instead of starting over
    let(:cache_policy_a) { modified_policy(name: "cache", rev_id: "a" * 40) }
    let(:cache_policy_b) { modified_policy(name: "cache", rev_id: "b" * 40) }

    let(:assoc_app_dev) { assoc(app_policy_1, "dev") }
    let(:assoc_db_dev) { assoc(db_policy_6, "dev") }
    let(:assoc_cache_dev) { assoc(cache_policy_a, "dev") }

    let(:assoc_app_test) { assoc(app_policy_2, "test") }
    let(:assoc_db_test) { assoc(db_policy_7, "test") }
    let(:assoc_cache_test) { assoc(cache_policy_b, "test") }

    let(:assoc_app_prod) { assoc(app_policy_3, "prod") }
    let(:assoc_db_prod) { assoc(db_policy_8, "prod") }

    let(:expected_dev_group_data) do
      {
        "uri" => api_url("policy_groups/dev"),
        "policies" => {
          "db" => { "revision_id" => "6666666666666666666666666666666666666666" },
          "appserver" => { "revision_id" => "1111111111111111111111111111111111111111" },
          "cache" => { "revision_id" => "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" }
        }
      }
    end

    let(:expected_test_group_data) do
      {
        "uri" => api_url("policy_groups/test"),
        "policies" => {
          "db" => { "revision_id" => "7777777777777777777777777777777777777777" },
          "appserver" => { "revision_id" => "2222222222222222222222222222222222222222" },
          "cache" => { "revision_id" => "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" }
        }
      }
    end

    let(:expected_prod_group_data) do
      {
        "uri" => api_url("policy_groups/prod"),
        "policies" => {
          "db" => { "revision_id" => "8888888888888888888888888888888888888888" },
          "appserver" => { "revision_id" => "3333333333333333333333333333333333333333" }
        }
      }
    end

    let(:request_payload) { nil }

    let(:request_method) { :GET }

    before do
      push(assoc_app_dev)
      push(assoc_db_dev)
      push(assoc_cache_dev)

      push(assoc_app_test)
      push(assoc_db_test)
      push(assoc_cache_test)

      push(assoc_app_prod)
      push(assoc_db_prod)
    end

    after do
      rm(assoc_app_dev)
      rm(assoc_db_dev)
      rm(assoc_cache_dev)

      rm(assoc_app_test)
      rm(assoc_db_test)
      rm(assoc_cache_test)

      rm(assoc_app_prod)
      rm(assoc_db_prod)
    end

    it "GET /policy_groups returns a data structure with all policy group->policy rev associations" do
      expect(response.code).to eq(200)
      response_obj = parse(response.body)
      expect(response_obj).to be_a_kind_of(Hash)
      expect(response_obj["dev"]).to eq(expected_dev_group_data)
      expect(response_obj["test"]).to eq(expected_test_group_data)
      expect(response_obj["prod"]).to eq(expected_prod_group_data)
    end

  end
end
