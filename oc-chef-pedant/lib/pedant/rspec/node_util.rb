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

require 'pedant/request'
require 'rspec/core/shared_context'

module Pedant
  module RSpec
    module NodeUtil
      extend ::RSpec::Core::SharedContext
      extend ::Pedant::Concern

      module ClassMethods
        # Call this to test several aspects of node creation.
        # Verifies proper status code and body for creation request,
        # as well as the fact that the created node can be retrieved.
        #
        # Creates several `it` blocks; should be called in a context
        # that has values for `request_method` (must be POST),
        # `request_url` (must be to the /nodes endpoint),
        # `request_payload`, and `requestor`, as well as `node_name`
        def should_successfully_create_a_node(message=nil)
          it 'should respond with 201 and the correct path' do
            request_method.should == :POST
            request_url.should == api_url("/nodes")
            should look_like create_node_success_response
          end
          it "should persist the node#{message ? ' ' + message : ''}" do
            request_payload['name'].should == node_name
            response.should look_like created_response
            get(api_url("/nodes/#{node_name}"), requestor).should look_like fetch_node_success_response
          end
        end

        # Validation helpers for node attributes, though you will need to include the Validations mixin
        def validates_node_attribute(attribute)
          context "when validating node attribute '#{attribute}'" do
            let(:validate_attribute) { attribute }
            optionally_accepts_value with: { 'valid_key' => 'valid value' }, default: {}

            accepts_valid_value   with: { 'valid_key' => 'valid value' }
            accepts_valid_value   with: { 'valid_key' => '' }
            accepts_valid_value   with: { 'valid_key' => nil }
            accepts_valid_value   with: { 'valid_key' => "これは日本語だ" }
            accepts_valid_value   with: { 'valid_key' => "\u0048\u0065\u006c\u006c\u006f\u0020\u65e5\u672c!" }
            accepts_valid_value   with: { 'valid_key' => 1 }
            accepts_valid_value   with: { 'valid_key' => [1, 2, 3] }
            accepts_valid_value   with: { 'valid_key' => ['a', 'b', 'c'] }
            accepts_valid_value   with: { 'valid_key' => { 'a' => 1, 'b' => 2 } }
            accepts_valid_value   with: { 'valid_key' => { 1 => 1, 2 => 2 } }
            accepts_valid_value   with: { '正標' => 'valid value' }
            accepts_valid_value   with: { '' => 1 } # Wha?
            rejects_invalid_value 1,  error_message: "Field '#{attribute}' is not a hash"
            rejects_invalid_value [], error_message: "Field '#{attribute}' is not a hash"
            rejects_invalid_value '', error_message: "Field '#{attribute}' is not a hash"
          end

        end

        def validates_run_list
          context "when validating 'run_list'" do
            let(:validate_attribute) { 'run_list' }
            optionally_accepts_value ['chef'], default: []

            # Run list must contain valid unqualified recipe name, qualified recipe name, or qualified role name
            accepts_valid_value  ['base', 'recipe[base]', 'role[base]']

            context 'with unqualified recipe names' do
              accepts_valid_value   ['base', 'nginx', 'app']
              accepts_valid_value   ['1', '2', '3']
              accepts_valid_value   ['base@1.0']
              accepts_valid_value   ['base@1.0.1']
              accepts_valid_value   ['base@1.0.1', 'nginx']

              rejects_invalid_value ['gibberish@1'],                       error_message: "Field 'run_list' is not a valid run list"
              rejects_invalid_value ['漢字', 'ひらがな', '한문', 'संस्कृतम्'], error_message: "Field 'run_list' is not a valid run list"
            end # with unqualified recipe names

            context 'with qualified recipe names' do
              accepts_valid_value   ['recipe[base]']
              accepts_valid_value   ['recipe[1]']
              accepts_valid_value   ['recipe[base@1.0]']
              accepts_valid_value   ['recipe[base@1.0.1]']
              accepts_valid_value   ['recipe[base]', 'recipe[base@1.0.1]']

              rejects_invalid_value ['recipe[gibberish@1]'], error_message: "Field 'run_list' is not a valid run list"
              rejects_invalid_value ['recipe[漢字]'],        error_message: "Field 'run_list' is not a valid run list"
            end # with qualified recipe names

            context 'with qualified role names' do
              accepts_valid_value   ['role[base]']
              accepts_valid_value   ['role[1]']

              rejects_invalid_value ['role[gibberish@1.0]'], error_message: "Field 'run_list' is not a valid run list"
              rejects_invalid_value ['role[漢字]'],          error_message: "Field 'run_list' is not a valid run list"
            end # with qualified role names

            context 'with invalid run_list items' do
              rejects_invalid_value [''],                error_message: "Field 'run_list' is not a valid run list"
              rejects_invalid_value ['node[gibberish]'], error_message: "Field 'run_list' is not a valid run list"
              rejects_invalid_value ['[gibberish]#@$%'], error_message: "Field 'run_list' is not a valid run list"
            end

            # Run list must be an array of strings
            context 'when not an array of strings' do
              rejects_invalid_value [1, 2, 3],      error_message: "Field 'run_list' is not a valid run list"
              rejects_invalid_value [[]],           error_message: "Field 'run_list' is not a valid run list"
              rejects_invalid_value ["string", []], error_message: "Field 'run_list' is not a valid run list"
              rejects_invalid_value Hash.new,       error_message: "Field 'run_list' is not a valid run list"
              rejects_invalid_value "string",       error_message: "Field 'run_list' is not a valid run list"
              rejects_invalid_value 1,              error_message: "Field 'run_list' is not a valid run list"
            end
          end
        end
      end

      # When you include this context, 'node_name' is set to the name
      # of the testing node, and 'node' is set to the Ruby Hash
      # representation of the node
      shared_context 'with temporary testing node' do
        let(:node_name) { 'pedant_node_test' }
        let(:node_environment){"_default"}
        let(:node_override){ {} }
        let(:node_default){ {} }
        let(:node_normal){ {} }
        let(:node_automatic){ {} }
        let(:node_run_list){ [] }
        let(:node) do
          {
            'name' => node_name,
            'json_class' => "Chef::Node",
            'chef_type' => 'node',
            'chef_environment' => node_environment,
            'override' => node_override,
            'normal' => node_normal,
            'default' => node_default,
            'automatic' => node_automatic,
            'run_list' => node_run_list
          }
        end

        before :each do
          add_node(admin_requestor, node)
        end

        after :each do
          delete_node(admin_requestor, node_name)
        end
      end # shared context

      let(:node_not_found_message) { ["node '#{node_name}' not found"] }

      let(:node_not_found_response) do
        {
          :status => 404,
          :body_exact => {"error" => node_not_found_message}
        }
      end

      let(:create_node_success_response) do
        {
          :status => 201,
          :body_exact => {
            "uri" => api_url("/nodes/#{node_name}")
          }
        }
      end

      let(:create_node_conflict_response) do
        {
          :status => 409,
          :body_exact => {
            "error" => ["Node already exists"]
          }
        }
      end

      let(:fetch_node_success_response) do
        {
          :status => 200,
          :body => normalize_node(node)
        }
      end

      let(:delete_node_success_response) do
        {
          :status => 200,
          :body => node
        }
      end

      let(:head_success_response) do
        {
          :status => 200
        }
      end

      let(:fetch_node_list_empty_response) do
        {
          :status => 200,
          :body_exact => {}
        }
      end

      let(:node_update_wrong_json_class_response) do
        {
          :status => 400,
          :body_exact => {
            "error" => ["Field 'json_class' invalid"]
          }
        }
      end

      # Ensure that the node's run list has been normalized
      def normalize_node(node)
        run_list = node['run_list'] || []
        node['run_list'] = normalize_run_list(run_list)
        node
      end

      def add_node(requestor, node)
        post(api_url("/nodes"), requestor, :payload => node)
      end

      def delete_node(requestor, name)
        delete(api_url("/nodes/#{name}"), requestor)
      end

      def new_node(name, opts={})
        {
          "name" => name,
          "json_class" => "Chef::Node",
          "chef_type" => "node",
          "chef_environment" => "_default",
          "override" => {},
          "normal" => {"is_anyone" => "no"},
          "default" => {},
          "automatic" => {},
          "run_list" => []
        }.merge(opts.stringify_keys)
      end

      def node_list_response(names)
        names.inject({}) { |h, name| h[name] = api_url("/nodes/#{name}"); h }
      end

    end
  end
end
