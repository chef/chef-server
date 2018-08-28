# -*- coding: utf-8 -*-
# Copyright: Copyright 2012-2018 Chef Software, Inc.
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

require 'pedant/rspec/validations'
require 'pedant/rspec/node_util'

describe "Testing the Nodes API endpoint", :nodes do
  include Pedant::RSpec::NodeUtil

  let(:admin_requestor){admin_user}
  let(:requestor){admin_requestor}

  let(:nonexistent_node_name) { "nonexistent_pedant_node" }

  context 'GET /nodes' do
    let(:request_method) { :GET }
    let(:request_url)    { api_url("/nodes") }

    context 'with no nodes on the server' do
      it 'returns a 200 with an empty hash' do
        should look_like fetch_node_list_empty_response
      end
    end

    max_number_of_nodes = 7
    (1..max_number_of_nodes).each do |num|
      context "with #{num} nodes on the server" do
        let(:nodes) do
          (1..num).map{|i| new_node(unique_name("pedant_node_list_test_#{i}"))}
        end

        before :each do
          nodes.each do |n|
            add_node(admin_requestor, n)
          end
        end

        after :each do
          nodes.each do |n|
            delete_node(admin_requestor, n['name'])
          end
        end

        it "should return a hash with #{num} nodes" do
          names = nodes.map{|n| n['name']}
          should look_like({
            :status => 200,
            :body_exact => node_list_response(names)
          })
        end
      end

    end
  end # GET /nodes

  context 'GET /nodes/<name>' do
    let(:request_method) { :GET }
    let(:request_url)    { api_url("/nodes/#{node_name}") }

    context 'for a nonexistent node' do
      let(:node_name){nonexistent_node_name}
      it 'returns a 404' do
        should look_like node_not_found_response
      end
    end

    context 'for an existing node' do
      include_context 'with temporary testing node'
      it 'returns a 200 and the node', :smoke do
        should look_like fetch_node_success_response
      end
    end
  end # GET /nodes/<name>

  context 'HEAD /nodes/<name>' do
    let(:request_method) { :HEAD }
    let(:request_url)    { api_url("/nodes/#{node_name}") }

    context 'for a nonexistent node' do
      let(:node_name){nonexistent_node_name}
      it 'returns a 404', :smoke do
        should look_like http_404_response
      end
    end

    context 'for an existing node' do
      include_context 'with temporary testing node'
      it 'returns a 200', :smoke do
        should look_like head_success_response
      end
    end
  end # HEAD /nodes/<name>

  context 'GET /environments/<environment_name>/nodes' do
    let(:request_method) { :GET }
    let(:request_url)    { api_url("/environments/#{environment_name}/nodes") }

    let(:environment_name){"_default"}

    context 'with no nodes on the server' do
      it 'returns a 200 with an empty hash' do
        should look_like fetch_node_list_empty_response
      end
    end
  end # GET /environments/<environment_name>/nodes

  context 'POST /nodes' do
    include Pedant::RSpec::Validations::Create

    let(:request_method) { :POST }
    let(:request_url) { api_url "/nodes" }
    let(:node) { fail 'Must define a "node" for POST tests!'}
    let(:request_payload){ node }
    let(:node_name) { unique_name('testing_node' ) }

    let(:resource_url) { api_url "/nodes/#{node_name}" }
    let(:default_resource_attributes) { new_node(node_name) }
    let(:persisted_resource_response) { get(resource_url, platform.admin_client) }

    after :each do
      delete_node(admin_requestor, node_name)
    end

    context 'when validating' do
      after(:each) { delete_node platform.admin_user, resource_name }
      let(:resource_url) { api_url "/nodes/#{resource_name}" }

      context "when validating 'name' field" do
        let(:validate_attribute) { 'name' }
        validates_existence_of 'name', skip_persistance_test: true

        accepts_valid_value   'pedant_node'
        accepts_valid_value   'PEDANT_NODE'
        accepts_valid_value   'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqurstuvwxyz0123456789-_:'
        rejects_invalid_value 'node@127.0.0.1'
      end

      context "when validating 'chef_environment' field" do
        let(:validate_attribute) { 'chef_environment' }
        optionally_accepts 'chef_environment', with: 'pedant'

        accepts_valid_value   'PEDANT'
        accepts_valid_value   'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqurstuvwxyz0123456789-_'
        rejects_invalid_value 'pedant:no_colon_in_environment_name'
        rejects_invalid_value 'pedant@127.0.0.1'
      end

      context "when validating 'json_class' field" do
        let(:validate_attribute) { 'json_class' }
        optionally_accepts 'json_class', with: 'Chef::Node', default: 'Chef::Node'

        rejects_invalid_value 'anything_else'
      end

      context "when validating 'chef_type' field" do
        let(:validate_attribute) { 'chef_type' }
        optionally_accepts 'chef_type', with: 'node', default: 'node'

        rejects_invalid_value 'anything_else'
      end

      context "when validating 'policy_name' field" do

        let(:default_resource_attributes) { new_node(node_name) }

        let(:request_payload) do
          new_node(node_name).tap do |node_data|
            node_data["policy_name"] = policy_name
          end
        end

        shared_examples_for "valid_policy_name" do

          it "accepts the policy_name field" do
            response = post(request_url, requestor, payload: request_payload)
            expect(response.code).to eq(201)

            get_response = get(resource_url, requestor)

            expect(get_response.code).to eq(200)
            returned_resource = parse(get_response.body)
            expect(returned_resource["policy_name"]).to eq(policy_name)
          end

        end

        context "when policy_name is valid" do

          context "with a simple valid policy name" do

            let(:policy_name) { "example-policy" }

            include_examples "valid_policy_name"

          end

          context "with a maximum length policy name" do

            let(:policy_name) { "d" * 255 }

            include_examples "valid_policy_name"

          end

          context "with a policy name containing every valid character" do

            let(:policy_name) { 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqurstuvwxyz0123456789-_:.' }

            include_examples "valid_policy_name"

          end

        end

        context "when policy name is invalid", :validation do

          shared_examples_for "invalid_policy_name" do

            let(:expected_response) { "{\"error\":[\"Field 'policy_name' invalid\"]}" }

            it "rejects the policy_name field" do
              response = post(request_url, requestor, payload: request_payload)
              expect(response.code).to eq(400)
              expect(response.body).to eq(expected_response)
            end

          end

          context "when it's not a string" do

            let(:policy_name) { 42 }

            include_examples "invalid_policy_name"

          end

          context "when it's too long" do

            let(:policy_name) { "F" * 256 }

            include_examples "invalid_policy_name"

          end

          [ ' ', '+', '!' ].each do |invalid_char|
            context "because the policy_name contains invalid character #{invalid_char}" do

              let(:policy_name) { "invalid-char" + invalid_char }

              include_examples "invalid_policy_name"

            end
          end
        end

      end

      context "when validating 'policy_group' field" do

        let(:default_resource_attributes) { new_node(node_name) }

        let(:request_payload) do
          new_node(node_name).tap do |node_data|
            node_data["policy_group"] = policy_group
          end
        end

        shared_examples_for "valid_policy_group" do

          it "accepts the policy_group field" do
            response = post(request_url, requestor, payload: request_payload)
            expect(response.code).to eq(201)

            get_response = get(resource_url, requestor)

            expect(get_response.code).to eq(200)
            returned_resource = parse(get_response.body)
            expect(returned_resource["policy_group"]).to eq(policy_group)
          end

        end

        context "when policy_group is valid" do

          context "with a simple valid policy name" do

            let(:policy_group) { "example-policy" }

            include_examples "valid_policy_group"

          end

          context "with a maximum length policy name" do

            let(:policy_group) { "d" * 255 }

            include_examples "valid_policy_group"

          end

          context "with a policy name containing every valid character" do

            let(:policy_group) { 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqurstuvwxyz0123456789-_:.' }

            include_examples "valid_policy_group"

          end

        end

        context "when policy name is invalid", :validation do

          shared_examples_for "invalid_policy_group" do

            let(:expected_response) { "{\"error\":[\"Field 'policy_group' invalid\"]}" }

            it "rejects the policy_group field" do
              response = post(request_url, requestor, payload: request_payload)
              expect(response.code).to eq(400)
              expect(response.body).to eq(expected_response)
            end

          end

          context "when it's not a string" do

            let(:policy_group) { 42 }

            include_examples "invalid_policy_group"

          end

          context "when it's too long" do

            let(:policy_group) { "F" * 256 }

            include_examples "invalid_policy_group"

          end

          [ ' ', '+', '!' ].each do |invalid_char|
            context "because the policy_group contains invalid character #{invalid_char}" do

              let(:policy_group) { "invalid-char" + invalid_char }

              include_examples "invalid_policy_group"

            end
          end
        end
      end

      validates_node_attribute 'normal'
      validates_node_attribute 'default'
      validates_node_attribute 'override'
      validates_node_attribute 'automatic'

      validates_run_list

      rejects_invalid_keys
    end

    context 'without existing node name', :smoke do
      let(:expected_response) { resource_created_exact_response }
      let(:created_resource) { { 'uri' => resource_url }  }

      let(:node) { new_node(node_name) }

      should_respond_with 201

    end # without existing node name

    context 'with existing node name' do
      include_context 'with temporary testing node'
      let(:expected_response) { conflict_exact_response }
      let(:conflict_error_message) { [ "Node already exists" ] }

      should_respond_with 409
    end

    unless Pedant.config['old_runlists_and_search']
      test_run_list_corner_cases :node
    end

    respects_maximum_payload_size

  end # POST /nodes

  context 'PUT /nodes/<name>' do
    include Pedant::RSpec::Validations::Update

    let(:request_method)  { :PUT }
    let(:request_url)     { api_url "/nodes/#{node_name}" }
    let(:resource_url)    { api_url "/nodes/#{node_name}" }
    let(:node_name)       { 'pedant_node_test' }

    let(:minimal_node_update) do
      {
        "json_class" => "Chef::Node",
        "run_list" => []
      }
    end

    context 'without an existing node' do
      let(:expected_response) { not_found_response }

      let(:node_name) { nonexistent_node_name }
      let(:request_payload) { minimal_node_update }

      should_respond_with 404
    end

    context 'with existing node' do
      include_context 'with temporary testing node'

      let(:default_resource_attributes) { node }
      let(:required_attributes) { } # PUT /nodes works like PATCH
      let(:original_resource_attributes) { node }
      let(:persisted_resource_response) { get(resource_url, platform.admin_user) }

      # Node names are fixed, once created cannot be renamed
      # If you pass 'name' in the update, it must match the URL
      context "when updating 'name' field" do
        let(:validate_attribute) { 'name' }
        optionally_accepts_value 'pedant_node_test', default: 'pedant_node_test'

        rejects_invalid_value 'pedant_node', error_message: "Node name mismatch."
        rejects_invalid_value 'PEDANT_NODE', error_message: "Node name mismatch."
        rejects_invalid_value 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqurstuvwxyz0123456789-_:', error_message: "Node name mismatch."
        rejects_invalid_value 'node@127.0.0.1', error_message: "Node name mismatch."
      end

      context "when validating 'chef_environment' field" do
        let(:validate_attribute) { 'chef_environment' }
        optionally_accepts_value 'pedant', default: '_default'

        accepts_valid_value   'PEDANT'
        accepts_valid_value   'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqurstuvwxyz0123456789-_'
        rejects_invalid_value 'pedant:no_colon_in_environment_name'
        rejects_invalid_value 'pedant@127.0.0.1'
      end

      context "when validating 'json_class' field" do
        let(:validate_attribute) { 'json_class' }
        optionally_accepts_value 'Chef::Node', default: 'Chef::Node'

        rejects_invalid_value 'anything_else'
      end

      context "when validating 'chef_type' field" do
        let(:validate_attribute) { 'chef_type' }
        optionally_accepts_value 'node', default: 'node'

        rejects_invalid_value 'anything_else'
      end

      validates_node_attribute 'normal'
      validates_node_attribute 'default'
      validates_node_attribute 'override'
      validates_node_attribute 'automatic'

      validates_run_list

      rejects_invalid_keys

      context "when validating 'policy_name' field" do

        let(:default_resource_attributes) { new_node(node_name) }

        let(:request_payload) do
          new_node(node_name).tap do |node_data|
            node_data["policy_name"] = policy_name
          end
        end

        shared_examples_for "valid_policy_name" do

          it "accepts the policy_name field" do
            response = put(request_url, requestor, payload: request_payload)
            expect(response.code).to eq(200)

            get_response = get(resource_url, requestor)

            expect(get_response.code).to eq(200)
            returned_resource = parse(get_response.body)
            expect(returned_resource["policy_name"]).to eq(policy_name)
          end

        end

        context "when policy_name is valid" do

          context "with a simple valid policy name" do

            let(:policy_name) { "example-policy" }

            include_examples "valid_policy_name"

          end

          context "with a maximum length policy name" do

            let(:policy_name) { "d" * 255 }

            include_examples "valid_policy_name"

          end

          context "with a policy name containing every valid character" do

            let(:policy_name) { 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqurstuvwxyz0123456789-_:.' }

            include_examples "valid_policy_name"

          end

        end

        context "when policy name is invalid", :validation do

          shared_examples_for "invalid_policy_name" do

            let(:expected_response) { "{\"error\":[\"Field 'policy_name' invalid\"]}" }

            it "rejects the policy_name field" do
              response = put(request_url, requestor, payload: request_payload)
              expect(response.code).to eq(400)
              expect(response.body).to eq(expected_response)
            end

          end

          context "when it's not a string" do

            let(:policy_name) { 42 }

            include_examples "invalid_policy_name"

          end

          context "when it's too long" do

            let(:policy_name) { "F" * 256 }

            include_examples "invalid_policy_name"

          end

          [ ' ', '+', '!' ].each do |invalid_char|
            context "because the policy_name contains invalid character #{invalid_char}" do

              let(:policy_name) { "invalid-char" + invalid_char }

              include_examples "invalid_policy_name"

            end
          end
        end

      end

      context "when validating 'policy_group' field" do

        let(:default_resource_attributes) { new_node(node_name) }

        let(:request_payload) do
          new_node(node_name).tap do |node_data|
            node_data["policy_group"] = policy_group
          end
        end

        shared_examples_for "valid_policy_group" do

          it "accepts the policy_group field" do
            response = put(request_url, requestor, payload: request_payload)
            expect(response.code).to eq(200)

            get_response = get(resource_url, requestor)

            expect(get_response.code).to eq(200)
            returned_resource = parse(get_response.body)
            expect(returned_resource["policy_group"]).to eq(policy_group)
          end

        end

        context "when policy_group is valid" do

          context "with a simple valid policy name" do

            let(:policy_group) { "example-policy" }

            include_examples "valid_policy_group"

          end

          context "with a maximum length policy name" do

            let(:policy_group) { "d" * 255 }

            include_examples "valid_policy_group"

          end

          context "with a policy name containing every valid character" do

            let(:policy_group) { 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqurstuvwxyz0123456789-_:.' }

            include_examples "valid_policy_group"

          end

        end

        context "when policy name is invalid", :validation do

          shared_examples_for "invalid_policy_group" do

            let(:expected_response) { "{\"error\":[\"Field 'policy_group' invalid\"]}" }

            it "rejects the policy_group field" do
              response = put(request_url, requestor, payload: request_payload)
              expect(response.code).to eq(400)
              expect(response.body).to eq(expected_response)
            end

          end

          context "when it's not a string" do

            let(:policy_group) { 42 }

            include_examples "invalid_policy_group"

          end

          context "when it's too long" do

            let(:policy_group) { "F" * 256 }

            include_examples "invalid_policy_group"

          end

          [ ' ', '+', '!' ].each do |invalid_char|
            context "because the policy_group contains invalid character #{invalid_char}" do

              let(:policy_group) { "invalid-char" + invalid_char }

              include_examples "invalid_policy_group"

            end
          end
        end
      end

      context 'with a canonical payload' do
        let(:existing_default_attributes){node['default']}
        let(:new_default_attributes){ {"foo" => "bar"}}

        let(:request_payload) do
          updated_node = {
            "default" => new_default_attributes,
            "json_class" => "Chef::Node",
          }
        end

        it 'updates the node', :smoke do
          # Just asserting what the existing default attributes are
          get(request_url, admin_requestor).should look_like({
            :status => 200,
            :body => {
            'name' => node_name,
            'default' => existing_default_attributes
          }
          })

          # make the change (implicit test subject 'response' actually triggers the call)
          should look_like({
            :status => 200,
            :body => {
            'name' => node_name,
            'default' => new_default_attributes
          }
          })

          # Verify the change happened
          get(request_url, admin_requestor).should look_like({
            :status => 200,
            :body => {
            'name' => node_name,
            'default' => new_default_attributes
          }
          })

        end
      end
    end

    respects_maximum_payload_size

  end # PUT /nodes/<name>

  context 'using DELETE' do
    let(:request_method) { :DELETE }
    let(:request_url)    { api_url("/nodes/#{node_name}") }

    context 'to a node that already exists' do
      include_context 'with temporary testing node'
      it 'succeeds', :smoke do
        ## TODO: Verify it's gone
        should look_like delete_node_success_response
      end
    end

    context 'to a node that does not exist' do
      let(:node_name){nonexistent_node_name}
      it 'fails' do
        should look_like node_not_found_response
      end
    end
  end # DELETE /nodes/<name>
end
