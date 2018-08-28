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
    module DataBagUtil
      extend ::RSpec::Core::SharedContext

      # Creates an empty testing data bag and cleans up afterward
      #
      # TODO: find clean way to have multiple testing data bags, and ones with items in them
      shared_context 'with testing data bag' do
        let(:temporary_data_bag_name){unique_name('temporary_bag')}

        before :each do
          create_data_bag(admin_requestor, new_data_bag(temporary_data_bag_name))
        end

        after :each do
          delete_data_bag(admin_requestor, temporary_data_bag_name)
        end

        let(:data_bag_name){temporary_data_bag_name}
      end

      shared_context 'with testing data bag items' do
        # Override this if you want to put things into a different bag
        let(:containing_data_bag){data_bag_name}
        let(:items){fail 'Please supply an array of complete data bag item hashes for \'items\''}

        before :each do
          items.each do |i|
            create_data_bag_item(admin_requestor, containing_data_bag, i)
          end
        end

        after :each do
          items.each do |i|
            delete_data_bag_item(admin_requestor, containing_data_bag, i['id'])
          end
        end

      end

      # Data Bag Responses and Messages
      ################################################################################

      # Can't do PUTs or DELETEs to /data
      let(:data_endpoint_method_not_allowed_response) do
        {
          :status => 405,
          :headers => {
            "allow" => ["GET, POST"]
          }
        }
      end

      let(:fetch_data_bag_list_empty_response) { http_200_response.with(:body_exact, {}) }

      let(:fetch_data_bag_list_full_response) do
        {
          :status => 200,
          :body_exact => {
            data_bag_1_name => api_url("/data/#{data_bag_1_name}"),

            data_bag_2_name => api_url("/data/#{data_bag_2_name}")
          }
        }
      end


      # Can't PUT to a specific data bag
      let(:data_bag_endpoint_method_not_allowed_response) do
        {
          :status => 405,
          :headers => {
            "allow" => ["GET, POST, DELETE"]
          }
        }
      end

      let(:fetch_empty_data_bag_success_response) { http_200_response.with(:body_exact, {}) }

      let(:fetch_full_data_bag_success_response) do
        {
          :status => 200,
          :body_exact => {
            data_bag_item_1_id => api_url("/data/#{data_bag_name}/#{data_bag_item_1_id}"),
            data_bag_item_2_id => api_url("/data/#{data_bag_name}/#{data_bag_item_2_id}"),
            data_bag_item_3_id => api_url("/data/#{data_bag_name}/#{data_bag_item_3_id}")
          }
        }
      end

      let(:data_bag_not_found_response) { http_404_response.with(:body_exact, "error" => ["Cannot load data bag #{data_bag_name}"] ) }

      let(:create_data_bag_success_response) { http_201_response.with(:body_exact, "uri" => api_url("/data/#{data_bag_name}")) }
      let(:create_data_bag_no_name_failure_response) { http_400_response.with(:body_exact, "error"=> ["Field 'name' missing"]) }
      let(:create_data_bag_bad_name_failure_response) { http_400_response.with(:body_exact, "error" => ["Field 'name' invalid"]) }
      let(:create_data_bag_conflict_response) { http_409_response.with(:body_Exact, "error" => ["Data bag already exists"]) }

      let(:delete_data_bag_success_response) do
        {
          :status => 200,
          :body_exact => {
            "name" => data_bag_name,
            "json_class" => "Chef::DataBag",
            "chef_type" => "data_bag"
          }
        }
      end

      let(:search_data_bag_no_data_bag_response) do
        {
          :status => 404,
          :body_exact => {
            "error" => ["I don't know how to search for #{data_bag_name} data objects."]
          }
        }
      end

      # Relies on 'data_bag_items' and 'data_bag_name' being set appropriately
      let(:search_data_bag_success_response) do
        {
          :status => 200,
          :body_exact => {
            "total" => data_bag_items.size,
            "start" => 0, # TODO: test this
            "rows" => data_bag_items.map do |i|
              search_result_databag_item(data_bag_name, i['id'], i) # TODO: clean up this signature!
            end
          }}
      end


      # Data Bag Item Responses and Messages
      ################################################################################

      # Can't POST to a specific data bag
      let(:data_bag_item_endpoint_method_not_allowed_response) do
        {
          :status => 405,
          :headers => {
            "allow" => ["GET, PUT, DELETE"]
          }
        }
      end

      let(:fetch_data_bag_item_success_response) { http_200_response.with :body_exact, data_bag_item }
      let(:fetch_updated_data_bag_item_success_response) { http_200_response.with :body_exact, updated_data_bag_item }

      # Data bag item not found responses have a bit of variation that needs to be eliminated
      let(:data_bag_item_not_found_message_1) { ["Cannot load data bag item #{data_bag_item_id} for data bag #{data_bag_name}"] }
      let(:data_bag_item_not_found_message_2) { ["Cannot load data bag #{data_bag_name} item #{data_bag_item_id}"] }

      # Can't get an item if the bag it's in doesn't exsit
      # TODO: Take this out when Open Source is on Erlang
      let(:data_bag_item_not_found_no_bag_response) { data_bag_item_not_found_response }
      let(:data_bag_item_not_found_from_delete_no_bag_response) { data_bag_item_not_found_from_delete_response }

      let(:data_bag_item_not_found_response) do
        http_404_response.with :body_exact, "error" => data_bag_item_not_found_message_1
      end

      let(:data_bag_item_not_found_from_delete_response) { http_404_response.with :body_exact, "error" => data_bag_item_not_found_message_2 }

      let(:data_bag_item_not_found_when_bag_is_deleted_response) { data_bag_item_not_found_response }
      let(:data_bag_item_not_found_from_put_response) { data_bag_item_not_found_response }


      let(:create_data_bag_item_no_id_response) { http_400_response.with :body_exact, "error" => ["Field 'id' missing"] }
      let(:create_data_bag_item_invalid_id_response) { http_400_response.with :body_exact, "error" => ["Field 'id' invalid"] }

      let(:create_data_bag_item_status_code) { 201 }

      let(:create_data_bag_item_success_response) do
        {
          :status => create_data_bag_item_status_code,
          :body_exact => with_extra_data_bag_item_fields(data_bag_item)
        }
      end

      let(:update_data_bag_item_success_response) do
        http_200_response.with :body_exact, with_extra_data_bag_item_fields(updated_data_bag_item)
      end

      let(:create_data_bag_item_no_data_bag_message) do
        ["No data bag '#{data_bag_name}' could be found. Please create this data bag before adding items to it."]
      end

      let(:create_data_bag_item_no_data_bag_response) { http_404_response.with :body_exact, "error" => create_data_bag_item_no_data_bag_message }

      let(:create_data_bag_item_conflict_message) do
        ["Data Bag Item '#{data_bag_item_id}' already exists in Data Bag '#{data_bag_name}'."]
      end

      let(:create_data_bag_item_conflict_response) { http_409_response.with :body_exact, "error" => create_data_bag_item_conflict_message }

      let(:update_data_bag_item_mismatched_id_response) { http_400_response.with :body_exact, "error" => ["DataBagItem name mismatch."] }

      # Even if the body is missing the ID, it'll get added back from the URL
      let(:update_data_bag_item_missing_id_response) do
        item = with_extra_data_bag_item_fields(updated_data_bag_item).with('id', data_bag_item_id)
        http_200_response.with :body_exact, item
      end

      let(:delete_data_bag_item_success_response) do
        {
          :status => 200,
          :body_exact => {
            "name" => "data_bag_item_#{data_bag_name}_#{data_bag_item_id}",
            "json_class" => "Chef::DataBagItem",
            "chef_type" => "data_bag_item",
            "data_bag" => data_bag_name,
            "raw_data" => data_bag_item
          }
        }
      end

      # Shared Contexts
      ################################################################################
      shared_context 'a successful data bag POST' do

        before :each do
          # Ensure it's not there before hand
          delete_data_bag(admin_user, data_bag_name)
          # Issue the POST and save the request for inspection later
          @request = post(data_bags_url, requestor, :payload => data_bag)
        end

        after :each do
          # Clean up after
          delete_data_bag(admin_user, data_bag_name)
        end

        it 'returns success' do
          @request.should look_like create_data_bag_success_response
        end
        it 'creates the data bag' do
          get(named_data_bag_url, requestor).should look_like fetch_empty_data_bag_success_response
        end
      end # end shared context

      shared_context 'an unsuccessful data bag POST' do
        before :each do
          @request = post(data_bags_url, requestor, :payload => data_bag)
        end

        it 'returns failure' do
          @request.should look_like expected_failure_response
        end
        it 'does not create a data bag' do
          get(named_data_bag_url, requestor).should look_like data_bag_not_found_response
        end
      end # end shared context

      shared_context 'a successful data bag item POST' do
        before :each do
          @response = post(named_data_bag_url, requestor, :payload => data_bag_item)
        end

        it 'returns success' do
          @response.should look_like create_data_bag_item_success_response
        end
        it 'creates the resource' do
          get(data_bag_item_url, requestor).should look_like fetch_data_bag_item_success_response
        end
      end # shared context

      shared_context 'an unsuccessful data bag item POST' do
        before :each do
          @response = post(named_data_bag_url, requestor, :payload => data_bag_item)
        end

        it 'returns failure' do
          @response.should look_like expected_failure_response
        end

        ## TODO: see if the item isn't there?
      end # shared context


      shared_context 'a successful data bag item PUT' do
        before :each do
          @response = put(data_bag_item_url, requestor, :payload => updated_data_bag_item)
        end

        it 'returns success' do
          @response.should look_like update_data_bag_item_success_response
        end

        it 'updates the data bag item' do
          get(data_bag_item_url, requestor).should look_like fetch_updated_data_bag_item_success_response
        end
      end # end shared context

      shared_context 'an unsuccessful data bag item PUT' do
        before :each do
          @response = put(data_bag_item_url, requestor, :payload => updated_data_bag_item)
        end

        it 'should return failure', :validation do
          @response.should look_like expected_failure_response
        end
      end # shared context

      # Helper Functions
      ################################################################################

      def with_extra_data_bag_item_fields(data_bag_item)
        should_be_hash(data_bag_item)
        i = data_bag_item.clone
        i['chef_type'] = 'data_bag_item'
        i['data_bag'] = data_bag_name
        i
      end

      def new_data_bag(name)
        should_be_string(name)
        {
          "name" => name,
          "json_class" => "Chef::DataBag",
          "chef_type" => "data_bag"
        }
      end

      def new_data_bag_item(id)
        should_be_string(id)
        {
          "id" => id,
          "foo" => "bar",
          "baz" => "quux"
        }
      end

      def create_data_bag_item(user, bag_name, item)
        should_be_string(bag_name)
        should_be_hash(item)
        post(api_url("/data/#{bag_name}"),
             user,
             :payload => item)
      end

      def delete_data_bag_item(user, bag_name, item_id)
        should_be_string(bag_name)
        should_be_string(item_id)
        begin
          delete(api_url("/data/#{bag_name}/#{item_id}"), user)
        rescue URI::InvalidURIError
          # ok
        end
      end

      def create_data_bag(user, bag)
        should_be_hash(bag)
        post(api_url("/data"),
             user,
             :payload => bag)
      end

      def delete_data_bag(user, name)
        should_be_string(name)
        delete(api_url("/data/#{name}"),
               user)
      end

    end
  end
end
