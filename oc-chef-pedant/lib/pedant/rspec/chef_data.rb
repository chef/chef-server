# Copyright: Copyright 2013-2018 Chef Software, Inc.
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

require 'pedant/concern'
require 'pedant/json'
require 'pedant/request'
require 'pedant/rspec/common_responses'
require 'pedant/rspec/http_status_codes'

module Pedant
  module RSpec
    module ChefData
      extend Pedant::Concern

      included do
        # Puts a client into the Chef server before the test, and deletes it at the end.
        # @param name [String] The name of the client
        # @param client [String] The JSON for the client
        # @example
        #   client 'x', '{ "name": "x" }'
        def self.client(name, client)
          before(:each) do
            post(api_url("clients/#{name}"), admin_user, :payload => client).should look_like({ :status => 201 })
          end
          after(:each) { delete(api_url("clients/#{name}"), admin_user) }
        end

        # Puts a data bag, with items, into the Chef server before the test, and
        # deletes it at the end.
        # @param name [String] The name of the data bag
        # @param data_bag [Hash] A hash containing the data bag items (name => JSON string)
        # @example
        #   data_bag 'x',
        #     'item1' => '{ "id": "item1" }',
        #     'item2' => '{ "id": "item2" }'
        def self.data_bag(name, data_bag)
          before(:each) do
            post(api_url("data"), admin_user, :payload => "{ \"name\": \"#{name}\" }").should look_like({ :status => 201 })
            data_bag.each do |item_name, item|
              post(api_url("data/#{name}"), admin_user, :payload => item).should look_like({ :status => 201 })
            end
          end
          after(:each) do
            data_bag.each do |item_name, item|
              delete(api_url("data/#{name}/#{item_name}"), admin_user)
            end
            delete(api_url("data/#{name}"), admin_user)
          end
        end

        # Puts an environment into the Chef server before the test, and deletes it at the end.
        # @param name [String] The name of the environment
        # @param environment [String] The JSON for the environment
        # @example
        #   environment 'x', '{ "name": "x" }'
        def self.environment(name, environment)
          before(:each) do
            post(api_url("environments"), admin_user, :payload => environment).should look_like({ :status => 201 })
          end
          after(:each) { delete(api_url("environments/#{name}"), admin_user) }
        end

        # Puts a node into the Chef server before the test, and deletes it at the end.
        # @param name [String] The name of the node
        # @param node [String] The JSON for the node
        # @example
        #   node 'x', '{ "name": "x" }'
        def self.node(name, node)
          before(:each) do
            post(api_url("nodes"), admin_user, :payload => node).should look_like({ :status => 201 })
          end
          after(:each) { delete(api_url("nodes/#{name}"), admin_user) }
        end

        # Puts a role into the Chef server before the test, and deletes it at the end.
        # @param name [String] The name of the role
        # @param role [String] The JSON for the role
        # @example
        #   role 'x', '{ "name": "x" }'
        def self.role(name, role)
          before(:each) do
            post(api_url("roles"), admin_user, :payload => role).should look_like({ :status => 201 })
          end
          after(:each) { delete(api_url("roles/#{name}"), admin_user) }
        end
      end
    end
  end
end
