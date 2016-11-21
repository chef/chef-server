# Copyright: Copyright (c) 2012 Opscode, Inc.
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

describe "Policies API endpoint", :policies do

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

  let(:policies_url) { api_url("/policies") }

  let(:policy_groups_url) { api_url("/policy_groups") }

  let(:static_named_policy_url) { api_url("/policy_groups/some_policy_group/policies/some_policy_name") }

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

  let(:canonical_policy_payload) do
    <<-PAYLOAD
      {
        "revision_id": "cf0885f3f2f5edaa44bf8d5e5de4c4d0efa51411",
        "name": "some_policy_name",
        "run_list": [
          "recipe[policyfile_demo::default]"
        ],
        "named_run_lists": {
          "update_jenkins": [
            "recipe[policyfile_demo::other_recipe]"
          ]
        },
        "cookbook_locks": {
          "policyfile_demo": {
            "version": "0.1.0",
            "identifier": "f04cc40faf628253fe7d9566d66a1733fb1afbe9",
            "dotted_decimal_identifier": "67638399371010690.23642238397896298.25512023620585",
            "source": "cookbooks/policyfile_demo",
            "cache_key": null,
            "scm_info": {
              "scm": "git",
              "remote": "git@github.com:danielsdeleo/policyfile-jenkins-demo.git",
              "revision": "edd40c30c4e0ebb3658abde4620597597d2e9c17",
              "working_tree_clean": false,
              "published": false,
              "synchronized_remote_branches": [

              ]
            },
            "source_options": {
              "path": "cookbooks/policyfile_demo"
            }
          }
        },
        "solution_dependencies": {
          "Policyfile": [
            [ "policyfile_demo", ">= 0.0.0" ]
          ],
          "dependencies": {
            "policyfile_demo (0.1.0)": []
          }
        }
      }
    PAYLOAD
  end

  let(:request_payload) { raise "define payload" }

  describe "Listing policies and policy groups" do

    let(:request_url) { policies_url }

    let(:list_policies) { get(policies_url, requestor) }

    let(:list_policy_groups) { get(policy_groups_url, requestor) }

    context "when no policies or policy_groups exist on the server" do

      it "GET /policies returns an empty list", :policy_name do
        expect(list_policies.code).to eq(200)
        response_obj = parse(list_policies.body)
        expect(response_obj).to be_a_kind_of(Hash)
        expect(response_obj).to eq({})
      end

      it "GET /policy_groups returns an empty list", :policy_groups do
        expect(list_policy_groups.code).to eq(200)
        response_obj = parse(list_policy_groups.body)
        expect(response_obj).to be_a_kind_of(Hash)
        expect(response_obj).to eq({})
      end

    end

    context "when a policy assigned to a group exists" do

      before do
        put(static_named_policy_url, requestor, payload: minimum_valid_policy_payload)
      end

      after(:each) do
        delete(static_named_policy_url, requestor)
      end

      let(:expected_policies_list) do
        {
          "some_policy_name" => {
            "uri" => api_url("policies/some_policy_name"),
            "revisions" => {
              "909c26701e291510eacdc6c06d626b9fa5350d25" => {}
            }
          }
        }
      end

      let(:expected_policy_group_list) do
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

      it "GET /policies returns a data structure with the groups assigned policy revision", :policy_name do
        expect(list_policies.code).to eq(200)
        response_obj = parse(list_policies.body)
        expect(response_obj).to be_a_kind_of(Hash)
        expect(response_obj).to eq(expected_policies_list)
      end

      it "GET /policy_groups returns a data structure with the groups assigned policy revision", :policy_groups do
        expect(list_policy_groups.code).to eq(200)
        response_obj = parse(list_policy_groups.body)
        expect(response_obj).to be_a_kind_of(Hash)
        expect(response_obj).to eq(expected_policy_group_list)
      end
    end

    context "policy exists but has been removed from all policy groups" do

      let(:expected_policy_list) do
        {
          "some_policy_name" => {
            "uri" => api_url("policies/some_policy_name"),
            "revisions" => {
              "909c26701e291510eacdc6c06d626b9fa5350d25" => {}
            }
          }
        }
      end

      let(:expected_policy_group_list) do
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

      it "GET /policies shows the policy", :policy_name do
        expect(list_policies.code).to eq(200)
        response_obj = parse(list_policies.body)
        expect(response_obj).to be_a_kind_of(Hash)
        expect(response_obj).to eq(expected_policy_list)
      end

      it "GET /policy_groups returns a data structure with the groups assigned policy revision", :policy_groups do
        expect(list_policy_groups.code).to eq(200)
        response_obj = parse(list_policy_groups.body)
        expect(response_obj).to be_a_kind_of(Hash)
        expect(response_obj).to eq(expected_policy_group_list)
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

      let(:expected_policies_list) do
        {
          "some_policy_name" => {
            "uri" => api_url("policies/some_policy_name"),
            "revisions" => {
              "909c26701e291510eacdc6c06d626b9fa5350d25" => {}
            }
          },
          "appserver" => {
            "uri" => api_url("policies/appserver"),
            "revisions" => {
              "1111111111111111111111111111111111111111" => {},
              "2222222222222222222222222222222222222222" => {},
              "3333333333333333333333333333333333333333" => {}
            }
          },
          "db" => {
            "uri" => api_url("policies/db"),
            "revisions" => {
              "6666666666666666666666666666666666666666" => {},
              "7777777777777777777777777777777777777777" => {},
              "8888888888888888888888888888888888888888" => {}
            }
          },
          "cache" => {
            "uri" => api_url("policies/cache"),
            "revisions" => {
              "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" => {},
              "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" => {}
            }
          }
        }
      end

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

      it "GET /policies returns a data structure with all the policies", :policy_name do
        expect(list_policies.code).to eq(200)
        response_obj = parse(list_policies.body)
        expect(response_obj).to be_a_kind_of(Hash)
        expect(response_obj).to eq(expected_policies_list)
      end

      it "GET /policy_groups returns a data structure with all policy group->policy rev associations", :policy_groups do
        expect(list_policy_groups.code).to eq(200)
        response_obj = parse(list_policy_groups.body)
        expect(response_obj).to be_a_kind_of(Hash)
        expect(response_obj["dev"]).to eq(expected_dev_group_data)
        expect(response_obj["test"]).to eq(expected_test_group_data)
        expect(response_obj["prod"]).to eq(expected_prod_group_data)
      end

    end
  end # describe "Listing policies and policy groups"

  describe "Named Policy Groups endpoint /policy_groups/:policy_group_name" do

    let(:named_policy_group_url) { api_url("/policy_groups/example-policy-group") }

    context "when the named group doesn't exist" do

      it "GET /policy_groups/:policy_group_name returns a 404" do
        expect(get(named_policy_group_url, requestor).code).to eq(404)
      end

      it "DELETE /policy_groups/:policy_group_name returns a 404" do
        expect(delete(named_policy_group_url, requestor).code).to eq(404)
      end

    end

    context "when the named group exists" do

      let(:associated_policy_url) { "#{named_policy_group_url}/policies/some_policy_name" }

      before do
        put(associated_policy_url, requestor, payload: minimum_valid_policy_payload)
      end

      after do
        delete(named_policy_group_url, requestor)
      end

      context "when no policies are assigned to the group" do

        let(:expected_body) do
          {
            "uri" => api_url("policy_groups/example-policy-group"),
            "policies" => {}
          }
        end

        before do
          # remove policy group association to clear out the policy group
          delete(associated_policy_url, requestor)
        end

        it "GET /policy_groups/:policy_group_name returns 200 with an empty JSON Object" do
          response = get(named_policy_group_url, requestor)
          expect(response.code).to eq(200)
          expect(parse(response.body)).to eq(expected_body)
        end

        it "DELETE /policy_groups/:policy_group_name returns 200 with an empty JSON Object" do
          response = delete(named_policy_group_url, requestor)
          expect(response.code).to eq(200)
          expect(parse(response.body)).to eq(expected_body)
        end

      end

      context "when policies are assigned to the group" do

        let(:expected_body) do
          {
            "uri" => api_url("policy_groups/example-policy-group"),
            "policies" => {
              "some_policy_name" => { "revision_id" => "909c26701e291510eacdc6c06d626b9fa5350d25" }
            }
          }
        end

        it "GET /policy_groups/:policy_group_name returns 200 with a JSON Object showing associated policies" do
          response = get(named_policy_group_url, requestor)
          expect(response.code).to eq(200)
          expect(parse(response.body)).to eq(expected_body)
        end

        it "DELETE /policy_groups/:policy_group_name returns 200 with a JSON Object showing associated policies" do
          response = delete(named_policy_group_url, requestor)
          expect(response.code).to eq(200)
          expect(parse(response.body)).to eq(expected_body)
        end

        it "DELETE /policy_groups/:policy_group_name deletes policy associations" do
          response = delete(named_policy_group_url, requestor)
          expect(response.code).to eq(200)

          expect(get(associated_policy_url, requestor).code).to eq(404)
        end

      end

    end

  end # describe "Named Policy Groups endpoint /policy_groups/:policy_group_name"

  describe "Named policy endpoint /policies/:policy_name" do

    let(:named_policy_url) { api_url("/policies/example-policy") }

    let(:named_policy_revisions_url) { api_url("/policies/example-policy/revisions") }

    let(:payload) do
      <<-PAYLOAD
      {
        "revision_id": "909c26701e291510eacdc6c06d626b9fa5350d25",
        "name": "example-policy",
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

    context "when :policy_name doesn't exist" do

      after do
        allowed_codes = [200, 404]
        actual = delete(named_policy_url, requestor).code
        # backwards, but there's no equal_one_of() in rspec
        expect(allowed_codes).to include(actual)
      end

      it "GET /policies/:policy_name returns a 404" do
        expect(get(named_policy_url, requestor).code).to eq(404)
      end

      it "DELETE /policies/:policy_name returns 404" do
        expect(delete(named_policy_url, requestor).code).to eq(404)
      end

      it "POST /policies/:policy_name/revisions creates the policy name and policy revision" do
        response = post(named_policy_revisions_url, requestor, payload: payload)
        expect(response.code).to eq(201)
        expect(parse(response.body)).to eq(parse(payload))
      end

      context "when the policy name in request doesn't match URL" do

        it "POST /policies/:policy_name/revisions returns 400", :validation do
          response = post(named_policy_revisions_url, requestor, payload: minimum_valid_policy_payload)
          expect(response.code).to eq(400)
        end
      end

    end

    context "when policy_name exists" do

      before do
        expect(post(named_policy_revisions_url, requestor, payload: payload).code).to eq(201)
      end

      after do
        allowed_codes = [200, 404]
        actual = delete(named_policy_url, requestor).code
        # backwards, but there's no equal_one_of() in rspec
        expect(allowed_codes).to include(actual)
      end

      let(:expected_policy_list) do
        {
          "revisions" => {
            "909c26701e291510eacdc6c06d626b9fa5350d25" => {},
          }
        }
      end

      it "GET /policies/:policy_name returns a list of revisions" do
        response = get(named_policy_url, requestor)
        expect(response.code).to eq(200)
        expect(parse(response.body)).to eq(expected_policy_list)
      end

      it "DELETE /policies/:policy_name deletes all revisions and returns the list of revisions" do
        response = delete(named_policy_url, requestor)
        expect(response.code).to eq(200)
        expect(parse(response.body)).to eq(expected_policy_list)

        expect(get(named_policy_url, requestor).code).to eq(404)
      end

      describe "creating a new revision" do

        let(:other_revision) do
          data = parse(payload).tap do |p|
            p["revision_id"] = "1111111111111111111111111111111111111111"
          end
          to_json(data)
        end

        context "when the revision doesn't yet exist" do

          it "POST /policies/:policy_name/revisions creates the policy revision" do
            response = post(named_policy_revisions_url, requestor, payload: other_revision)
            expect(response.code).to eq(201)
            expect(parse(response.body)).to eq(parse(other_revision))
          end

        end

        context "when the revision exists" do

          it "POST /policies/:policy_name/revisions returns 409" do
            response = post(named_policy_revisions_url, requestor, payload: payload)
            expect(response.code).to eq(409)
          end

        end

      end

    end

  end # describe "Named policy endpoint /policies/:policy_name"

  describe "Named policy revision endpoint /policies/:policy_name/revisions/:revision_id" do

    let(:named_policy_url) { api_url("/policies/example-policy") }

    let(:named_revision_url) { api_url("/policies/example-policy/revisions/909c26701e291510eacdc6c06d626b9fa5350d25") }

    let(:named_policy_revisions_url) { api_url("/policies/example-policy/revisions") }

    let(:payload) do
      <<-PAYLOAD
      {
        "revision_id": "909c26701e291510eacdc6c06d626b9fa5350d25",
        "name": "example-policy",
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

    context "when the policy_name doesn't exist" do

      it "GET /policies/:policy_name/revisions/:revision_id returns 404" do
        expect(get(named_revision_url, requestor).code).to eq(404)
      end

      it "DELETE /policies/:policy_name/revisions/:revision_id returns 404" do
        expect(delete(named_revision_url, requestor).code).to eq(404)
      end

    end

    context "when the policy_name exists but :revision_id doesn't" do

      let(:other_revision) do
        data = parse(payload).tap do |p|
          p["revision_id"] = "1111111111111111111111111111111111111111"
        end
        to_json(data)
      end

      let(:other_revision_url) { api_url("/policies/example-policy/revisions/1111111111111111111111111111111111111111") }

      before do
        expect(post(named_policy_revisions_url, requestor, payload: other_revision).code).to eq(201)
      end

      after do
        allowed_codes = [200, 404]
        actual = delete(named_policy_url, requestor).code
        # backwards, but there's no equal_one_of() in rspec
        expect(allowed_codes).to include(actual)
      end

      it "GET /policies/:policy_name/revisions/:revision_id returns 404" do
        expect(get(named_revision_url, requestor).code).to eq(404)
      end

      it "DELETE /policies/:policy_name/revisions/:revision_id returns 404" do
        expect(delete(named_revision_url, requestor).code).to eq(404)
      end

    end

    context "when the policy_name exists and :revision_id exists" do

      before do
        expect(post(named_policy_revisions_url, requestor, payload: payload).code).to eq(201)
      end

      after do
        allowed_codes = [200, 404]
        actual = delete(named_policy_url, requestor).code
        # backwards, but there's no equal_one_of() in rspec
        expect(allowed_codes).to include(actual)
      end

      it "GET /policies/:policy_name/revisions/:revision_id returns 200, with the policy as the body" do
        response = get(named_revision_url, requestor)
        expect(response.code).to eq(200)
        expect(parse(response.body)).to eq(parse(payload))
      end

      it "DELETE /policies/:policy_name/revisions/:revision_id returns 200, with the policy as the body" do
        response = delete(named_revision_url, requestor)
        expect(response.code).to eq(200)
        expect(parse(response.body)).to eq(parse(payload))
        expect(get(named_revision_url, requestor).code).to eq(404)
      end

      it "DELETE /policies/:policy_name/revisions/:revision_id doesn't delete authz information for :policy_name" do
        response = delete(named_revision_url, requestor)
        expect(response.code).to eq(200)
        expect(get(named_revision_url, requestor).code).to eq(404)
        post(named_policy_revisions_url, requestor, payload: payload)
        expect(get(named_revision_url, requestor).code).to eq(200)
      end

    end

  end # describe "Named policy revision endpoint /policies/:policy_name/revisions/:revision_id"


  describe "Policy Group Revision Association Endpoint /policy_groups/:policy_group/policies/:policy_name", :policy_group_assoc do

    let(:static_named_policy_url) { api_url("/policy_groups/some_policy_group/policies/some_policy_name") }

    let(:request_url) { static_named_policy_url }

    context "when no policies exist on the server" do

      context "GET" do

        let(:request_payload) { nil }

        let(:request_method) { :GET }

        it "GET /policy_groups/:group_name/policies/:policy_name returns 404" do
          expect(response.code).to eq(404)
        end

      end

      context "DELETE" do

        let(:request_payload) { nil }

        let(:request_method) { :DELETE }

        it "DELETE /policy_groups/:group_name/policies/:policy_name returns 404" do
          expect(response.code).to eq(404)
        end

      end

      context "PUT" do

        let(:request_method) { :PUT }

        after(:each) do
          delete(static_named_policy_url, requestor)
        end

        context "with a canonical payload" do

          let(:request_payload) { canonical_policy_payload }

          it "PUT /policy_groups/:group_name/policies/:policy_name returns 201" do
            expect(response.code).to eq(201)
          end


        end

        context "with a minimal payload" do

          let(:request_payload) { minimum_valid_policy_payload }

          it "PUT /policy_groups/:group_name/policies/:policy_name returns 201" do
            expect(response.code).to eq(201)
          end

        end

        context "with a payload demonstrating validation edge conditions for 'name'" do

          context "when the name contains every valid character" do
            let(:name_with_all_valid_chars) { 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqurstuvwxyz0123456789-_:.' }

            # Have to override the URL or else we will hit validation that name in
            # document matches the one in URL
            let(:static_named_policy_url) { api_url("/policy_groups/some_policy_group/policies/#{name_with_all_valid_chars}") }

            let(:request_payload) do
              mutate_json(minimum_valid_policy_payload) { |p| p["name"] = name_with_all_valid_chars }
            end

            it "PUT /policy_groups/:group_name/policies/:policy_name returns 201" do
              expect(response.code).to eq(201)
            end
          end

          context "when the name is close to the maximum size", :chef_zero_quirks do

            # On Chef Zero, some backends will append `.json` to a file name,
            # which can exceed the common limit of 255 characters.
            let(:max_size_name) { 'a' * 250 }

            # Have to override the URL or else we will hit validation that name in
            # document matches the one in URL
            let(:static_named_policy_url) { api_url("/policy_groups/some_policy_group/policies/#{max_size_name}") }

            let(:request_payload) do
              mutate_json(minimum_valid_policy_payload) { |p| p["name"] = max_size_name }
            end

            it "PUT /policy_groups/:group_name/policies/:policy_name returns 201" do
              expect(response.code).to eq(201)
            end
          end

          context "when a revision_id is the maximum size", :chef_zero_quirks do

            let(:max_size_revision_id) { 'a' * 255 }

            let(:request_payload) do
              mutate_json(minimum_valid_policy_payload) do |policy|
                policy["revision_id"] = max_size_revision_id
              end
            end

            it "PUT /policy_groups/:group_name/policies/:policy_name returns 201" do
              expect(response.code).to eq(201)
            end
          end

          context "when a revision_id contains every valid character" do

            let(:revision_id_with_all_valid_chars) { 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqurstuvwxyz0123456789-_:.' }

            let(:request_payload) do
              mutate_json(minimum_valid_policy_payload) do |policy|
                policy["revision_id"] = revision_id_with_all_valid_chars
              end
            end


            it "PUT /policy_groups/:group_name/policies/:policy_name returns 201" do
              expect(response.code).to eq(201)
            end
          end

          context "when a cookbook identifier is the maximum size" do

            let(:max_size_identifier) { 'a' * 255 }

            let(:request_payload) do
              mutate_json(minimum_valid_policy_payload) do |policy|
                policy["cookbook_locks"]["edge_case"] = {
                  "identifier" => max_size_identifier,
                  "version" => "1.2.3"
                }
              end
            end

            it "PUT /policy_groups/:group_name/policies/:policy_name returns 201" do
              expect(response.code).to eq(201)
            end
          end

          context "when a cookbook identifier contains every valid character" do

            let(:identifier_with_all_valid_chars) { 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqurstuvwxyz0123456789-_:.' }

            let(:request_payload) do
              mutate_json(minimum_valid_policy_payload) do |policy|
                policy["cookbook_locks"]["edge_case"] = {
                  "identifier" => identifier_with_all_valid_chars,
                  "version" => "1.2.3"
                }
              end
            end

            it "PUT /policy_groups/:group_name/policies/:policy_name returns 201" do
              expect(response.code).to eq(201)
            end
          end
        end

        context "when the request body is invalid" do

          shared_examples_for "an invalid policy document" do

            let(:error_message) do
              response_obj = parse(response.body)
              expect(response_obj).to be_a_kind_of(Hash)
              expect(response_obj).to have_key("error")
              expect(response_obj["error"]).to be_a_kind_of(Array)
              expect(response_obj["error"].size).to eq(1)
              response_obj["error"].first
            end

            it "PUT /policy_groups/:group_name/policies/:policy_name returns 400", :validation do
              expect(response.code).to eq(400)
            end

            it "PUT /policy_groups/:group_name/policies/:policy_name body contains a well-formed error message", :validation do
              expect(error_message).to eq(expected_error_message)
            end

          end

          ## MANDATORY FIELDS AND FORMATS
          # * `revision_id`: String; Must be < 255 chars, matches /^[\-[:alnum:]_\.\:]+$/
          # * `name`: String; Must match name in URI; Must be < 255 chars, matches /^[\-[:alnum:]_\.\:]+$/
          # * `run_list`: Array
          # * `run_list[i]`: Fully Qualified Recipe Run List Item
          # * `cookbook_locks`: JSON Object
          # * `cookbook_locks(key)`: CookbookName
          # * `cookbook_locks[item]`: JSON Object, mandatory keys: "identifier", "dotted_decimal_identifier"
          # * `cookbook_locks[item]["identifier"]`: varchar(255) ?
          # * `cookbook_locks[item]["dotted_decimal_identifier"]` ChefCompatibleVersionNumber

          context "because of missing revision id field" do

            let(:request_payload) do
              mutate_json(minimum_valid_policy_payload) { |p| p.delete("revision_id") }
            end

            let(:expected_error_message) { "Field 'revision_id' missing" }

            include_examples "an invalid policy document"

          end

          context "because revision id field is an empty string" do

            let(:request_payload) do
              mutate_json(minimum_valid_policy_payload) { |p| p["revision_id"] = "" }
            end

            let(:expected_error_message) { "Field 'revision_id' invalid" }

            include_examples "an invalid policy document"

          end

          context "because revision id field is larger than 255 characters" do

            let(:long_revision_id_is_long) { "f" * 256 }

            let(:request_payload) do
              mutate_json(minimum_valid_policy_payload) { |p| p["revision_id"] = long_revision_id_is_long }
            end

            let(:expected_error_message) { "Field 'revision_id' invalid" }

            include_examples "an invalid policy document"

          end

          [ ' ', '+', '!' ].each do |invalid_char|
            context "because the revision_id contains invalid character #{invalid_char}" do

              let(:invalid_revision_id) { "invalid" + invalid_char + "invalid" }

              let(:request_payload) do
                mutate_json(minimum_valid_policy_payload) { |p| p["revision_id"] = invalid_revision_id }
              end

              let(:expected_error_message) { "Field 'revision_id' invalid" }

              include_examples "an invalid policy document"

            end
          end


          context "because of missing name field" do

            let(:request_payload) do
              mutate_json(minimum_valid_policy_payload) { |p| p.delete("name") }
            end

            let(:expected_error_message) { "Field 'name' missing" }

            include_examples "an invalid policy document"

          end

          context "because of an mismatched name field" do

            let(:request_payload) do
              mutate_json(minimum_valid_policy_payload) { |p| p["name"] = "monkeypants" }
            end

            let(:expected_error_message) { "Field 'name' invalid : some_policy_name does not match monkeypants" }

            include_examples "an invalid policy document"

          end

          context "because the name is larger than 255 characters" do

            let(:long_name_is_long) { "z" * 256 }

            # Have to override the URL or else we might only hit validation that
            # name in document matches the one in URL
            let(:static_named_policy_url) { api_url("/policy_groups/some_policy_group/policies/#{long_name_is_long}") }

            let(:request_payload) do
              mutate_json(minimum_valid_policy_payload) { |p| p["name"] = long_name_is_long }
            end

            let(:expected_error_message) { "Field 'name' invalid" }

            include_examples "an invalid policy document"

          end

          [ ' ', '+', '!' ].each do |invalid_char|
            context "because the name contains invalid character '#{invalid_char}'" do

              let(:invalid_policy_name) { "invalid" + invalid_char + "invalid" }

              let(:encoded_invalid_name) { URI.encode(invalid_policy_name) }

              # Have to override the URL or else we might only hit validation that
              # name in document matches the one in URL
              let(:static_named_policy_url) { api_url("/policy_groups/some_policy_group/policies/#{encoded_invalid_name}") }

              let(:request_payload) do
                mutate_json(minimum_valid_policy_payload) { |p| p["name"] = invalid_policy_name }
              end

              let(:expected_error_message) { "Field 'name' invalid" }

              include_examples "an invalid policy document"

            end
          end

          context "because of missing run_list field" do

            let(:request_payload) do
              mutate_json(minimum_valid_policy_payload) { |p| p.delete("run_list") }
            end

            let(:expected_error_message) { "Field 'run_list' missing" }

            include_examples "an invalid policy document"

          end

          context "because run_list field is the wrong type" do

            let(:request_payload) do
              mutate_json(minimum_valid_policy_payload) { |p| p["run_list"] = {} }
            end

            let(:expected_error_message) { "Field 'run_list' is not a valid run list" }

            include_examples "an invalid policy document"

          end

          # Run list items in policies are required to be fully normalized recipe names, e.g,
          # "recipe[mysql::default]"
          [123, "recipe[", "role[foo]", "recipe[foo]"].each do |invalid_run_list_item|

            context "because the run_list has invalid item '#{invalid_run_list_item}'" do

              let(:request_payload) do
                mutate_json(minimum_valid_policy_payload) { |p| p["run_list"] = [ invalid_run_list_item ] }
              end

              let(:expected_error_message) { "Field 'run_list' is not a valid run list" }

              include_examples "an invalid policy document"

            end

          end

          context "because cookbook_locks field is missing" do

            let(:request_payload) do
              mutate_json(minimum_valid_policy_payload) { |p| p.delete("cookbook_locks") }
            end

            let(:expected_error_message) { "Field 'cookbook_locks' missing" }

            include_examples "an invalid policy document"

          end

          context "because cookbook_locks field is the wrong type" do

            let(:request_payload) do
              mutate_json(minimum_valid_policy_payload) { |p| p["cookbook_locks"] = [] }
            end

            let(:expected_error_message) { "Field 'cookbook_locks' invalid" }

            include_examples "an invalid policy document"

          end

          context "because cookbook_locks contains an entry of the wrong type" do

            let(:request_payload) do
              mutate_json(minimum_valid_policy_payload) { |p| p["cookbook_locks"]["invalid_member"] = [] }
            end

            let(:expected_error_message) { "Field 'cookbook_locks' invalid" }

            # TODO: customizing the 400 message for this is currently difficult
            # in erchef, so we skip validating the message.

            it "PUT /policies/:group/:name returns 400", :validation do
              expect(response.code).to eq(400)
            end

          end

          context "because cookbook_locks contains an entry that is missing the identifier field" do

            let(:request_payload) do
              mutate_json(minimum_valid_policy_payload) do |policy|
                policy["cookbook_locks"]["invalid_member"] = { "dotted_decimal_identifier" => "1.2.3" }
              end
            end

            let(:expected_error_message) { "Field 'identifier' missing" }

            include_examples "an invalid policy document"

          end

          context "because cookbook_locks contains an entry with an identifier larger than 255 characters" do

            let(:long_identifier) { "a" * 256 }

            let(:invalid_lock) do
              {
                "identifier" => long_identifier,
                "version" => "1.2.3"
              }
            end

            let(:request_payload) do
              mutate_json(minimum_valid_policy_payload) do |policy|
                policy["cookbook_locks"]["invalid_member"] = invalid_lock
              end
            end

            let(:expected_error_message) { "Field 'identifier' invalid" }

            include_examples "an invalid policy document"

          end


          context "because cookbook_locks contains an entry with an invalid dotted_decimal_identifier field" do

            let(:request_payload) do
              mutate_json(minimum_valid_policy_payload) do |policy|
                policy["cookbook_locks"]["invalid_member"] = { "identifier" => "123def", "version" => "1.2.3", "dotted_decimal_identifier" => "foo" }
              end
            end

            let(:expected_error_message) { "Field 'dotted_decimal_identifier' is not a valid version" }

            include_examples "an invalid policy document"

          end

        end

      end

    end

    context "when a policy exists on the server" do

      before(:each) do
        put(static_named_policy_url, requestor, payload: canonical_policy_payload)
      end

      context "GET" do

        let(:request_method) { :GET }

        let(:request_payload) { nil }

        it "retrieves the policy document" do
          expect(parse(response.body)).to eq(parse(canonical_policy_payload))
        end

      end

      context "PUT (update policy document)" do

        let(:updated_canonical_policy_payload) do
          mutate_json(canonical_policy_payload) do |policy|
            policy["revision_id"] = "d4991d020462724edcf05f572e1d856cc5927803"
            policy["cookbook_locks"]["policyfile_demo"]["identifier"] = "2a42abea88dc847bf6d3194af8bf899908642421"
            policy["cookbook_locks"]["policyfile_demo"]["dotted_decimal_identifier"] = "11895255163526276.34892808658286783.151290363782177"
          end
        end

        let(:request_payload) { updated_canonical_policy_payload }

        let(:request_method) { :PUT }

        before(:each) do
          # Force the update PUT to occur
          response
        end

        it "PUT /policies/:group/:name returns 200" do
          expect(response.code).to eq(200)
          expect(parse(response.body)).to eq(parse(updated_canonical_policy_payload))
        end

        it "GET /policies/:group/:name subsequently returns the updated document" do
          retrieved_doc = get(static_named_policy_url, requestor)
          expect(retrieved_doc.code).to eq(200)
          expect(parse(retrieved_doc.body)).to eq(parse(updated_canonical_policy_payload))
        end
      end

      context "DELETE" do

        let(:request_payload) { nil }

        let(:request_method) { :DELETE }

        before(:each) do
          # Force the DELETE to occur
          response
        end


        it "DELETE /policies/:group/:name returns the deleted document" do
          expect(response.code).to eq(200)
          expect(parse(response.body)).to eq(parse(canonical_policy_payload))
        end

        it "DELETE /policies/:group/:name removes the policy from the data store" do
          subsequent_get = get(static_named_policy_url, requestor)
          expect(subsequent_get.code).to eq(404)
        end


      end

    end

  end
end
