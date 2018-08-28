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

require 'pedant/rspec/data_bag_util'
describe "Data Bag API endpoint", :data_bags do
  include Pedant::RSpec::DataBagUtil

  # Just until we rename the requestors
  let(:admin_requestor){ admin_user }

  let(:requestor){admin_requestor}
  let(:data_bags_url){api_url("/data")}
  let(:named_data_bag_url){api_url("/data/#{data_bag_name}")}
  let(:data_bag_item_url){api_url("/data/#{data_bag_name}/#{data_bag_item_id}")}

  context 'with no data bags' do
    context 'a request to /data' do
      context 'GET' do
        it 'returns an empty list of data bags' do
          get(data_bags_url, requestor).should look_like fetch_data_bag_list_empty_response
        end
      end # GET
      context 'POST' do
        let(:request_method){:POST}

        let(:data_bag_name){unique_name("testbag")}
        let(:data_bag_url){api_url("/data/#{data_bag_name}")}
        let(:request_url){data_bag_url}
        let(:data_bag){new_data_bag(data_bag_name)}

        after :each do
          delete(data_bag_url, admin_requestor)
        end

        context 'with a canonical payload', :smoke do
          it_behaves_like 'a successful data bag POST'
        end

        context 'valid requests of various types to create a data bag' do
          context 'with a valid name' do
            names = ['pedant', 'pedant-bag', 'pedant_bag', 'pedant_bag-foo', '1234567890', 'pedant99', 'pedant:with:colons', 'pedant.with.dots']

            names.each do |n|
              context "like '#{n}'" do
                let(:data_bag_name){n}
                it_behaves_like 'a successful data bag POST'
              end
            end
          end

          context 'missing certain keys' do
            ['json_class', 'chef_type'].each do |key|
              context "without a '#{key}' key" do
                let(:data_bag) {
                  new_data_bag(data_bag_name).tap{|b| b.delete key}
                }
                it_behaves_like 'a successful data bag POST'
              end
            end
          end

          context 'with just a name' do
            let(:data_bag) {{ "name" => data_bag_name }}
            it_behaves_like 'a successful data bag POST'
          end

          context 'with an incorrect \'chef_type\' key' do
            let(:data_bag) {
              new_data_bag(data_bag_name).tap{|b| b['chef_type'] = 'node'}
            }
            it 'does not have the correct chef_type key' do
              # Just a touch of paranoia
              data_bag['chef_type'].should_not eq "data_bag"
            end
            it_behaves_like 'a successful data bag POST'
          end

          context 'with an incorrect \'json_class\' key' do
            let(:data_bag) {
              new_data_bag(data_bag_name).tap{|b| b['json_class'] = 'Chef::Node'}
            }
            it 'does not have the correct json_class key' do
              # Just a touch of paranoia
              data_bag['json_class'].should_not eq "Chef::DataBag"
            end
            it_behaves_like 'a successful data bag POST'
          end
        end # valid requests

        context 'invalid requests of various types to create a data bag', :validation do

          context 'with an invalid name' do
            bad_names = ["pedant_badName!!$$$$_oh_very+bad", "pedant-does-not-like-punctuation!!!!"]
            bad_names.each do |bad_name|
              context "like '#{bad_name}'" do
                let(:data_bag_name) { bad_name }
                let(:expected_failure_response) { create_data_bag_bad_name_failure_response }
                it_behaves_like 'an unsuccessful data bag POST'
              end
            end
          end

          context 'without a name' do
            let(:data_bag) {{ "id" => data_bag_name }}
            let(:expected_failure_response) { create_data_bag_no_name_failure_response }
            it_behaves_like 'an unsuccessful data bag POST'
          end
        end # invalid requests

        respects_maximum_payload_size
      end # POST

      context 'PUT' do
        let(:put_body){{"fake" => "value"}}
        it 'is not allowed' do
          put(data_bags_url, requestor, :payload => put_body).should look_like data_endpoint_method_not_allowed_response
        end
      end # PUT

      context 'DELETE' do
        it 'is not allowed' do
          delete(data_bags_url, requestor).should look_like data_endpoint_method_not_allowed_response
        end
      end # DELETE
    end # request to /data

    context 'a request to /data/<bag>' do
      let(:data_bag_name){unique_name("no_bag")}
      context 'GET' do
        it 'fails because there is no bag' do
          get(named_data_bag_url, requestor).should look_like data_bag_not_found_response
        end
      end
      context 'POST' do
        let(:post_body){{"id" => "blah"}}
        it 'fails because there is no bag' do
          post(named_data_bag_url, requestor, :payload => post_body).should look_like create_data_bag_item_no_data_bag_response
        end
      end
      context 'PUT' do
        let(:put_body){{"fake" => "value"}}
        it 'is not allowed' do
          put(named_data_bag_url, requestor,
              :payload => put_body).should look_like data_bag_endpoint_method_not_allowed_response
        end
      end
      context 'DELETE' do
        it 'fails because there is no bag' do
          delete(named_data_bag_url, requestor).should look_like data_bag_not_found_response
        end
      end
    end #  'a request to /data/<bag>'
    context 'a request to /data/<bag>/<item>' do
      let(:data_bag_name){unique_name("no_bag")}
      let(:data_bag_item_id){unique_name("no_item")}
      context 'GET' do
        it 'fails because there is no bag' do
          get(data_bag_item_url, requestor).should look_like data_bag_item_not_found_no_bag_response
        end
      end
      context 'POST' do
        let(:post_body){{"fake" => "value"}}
        it 'is not allowed' do
          post(data_bag_item_url, requestor, :payload => post_body).should look_like data_bag_item_endpoint_method_not_allowed_response
        end
      end
      context 'PUT' do
        let(:put_body){{"id"=>data_bag_item_id}}
        it 'fails because there is no bag' do
          put(data_bag_item_url, requestor, :payload => put_body).should look_like data_bag_item_not_found_no_bag_response
        end
      end
      context 'DELETE' do
        it 'fails because there is no bag' do
          delete(data_bag_item_url, requestor).should look_like data_bag_item_not_found_from_delete_no_bag_response
        end
      end
    end #  'a request to /data/<bag>'
  end # with no data bags
  context 'with data bags' do
    let(:data_bag_1_name){unique_name("bag1")}
    let(:data_bag_2_name){unique_name("bag2")}
    let(:data_bag_1){ new_data_bag(data_bag_1_name)}
    let(:data_bag_2){ new_data_bag(data_bag_2_name)}
    before :each do
      create_data_bag(admin_requestor, data_bag_1)
      create_data_bag(admin_requestor, data_bag_2)
    end
    after :each do
      delete_data_bag(admin_requestor, data_bag_1_name)
      delete_data_bag(admin_requestor, data_bag_2_name)
    end
    context 'that have no items' do
      context 'a request to /data' do
        context 'GET' do
          it 'returns a non-empty list of data bags' do
            get(data_bags_url, requestor).should look_like fetch_data_bag_list_full_response
          end
        end # GET
        context 'POST' do
          let(:data_bag_name){data_bag_1_name}
          let(:data_bag){data_bag_1}
          it 'raises a conflict when creating an existing data bag' do
            post(data_bags_url, requestor, :payload => data_bag_1).should look_like create_data_bag_conflict_response
          end
        end # POST
        # PUT isn't allowed, but that's already been tested
        # DELETE isn't allowed, but that's already been tested
      end # request to /data
      context 'a request to /data/<bag>' do
        let(:data_bag_name){data_bag_1_name}

        context 'GET' do
          it 'returns an empty bag' do
            get(named_data_bag_url, requestor).should look_like fetch_empty_data_bag_success_response
          end
        end
        context 'POST' do
          let(:request_method){:POST}
          let(:data_bag_item_id){unique_name("item")}
          let(:request_url){api_url("/data/#{data_bag_name}")}

          after :each do
            delete_data_bag_item(admin_requestor, data_bag_name, data_bag_item_id)
          end
          context 'various good inputs to create a data bag item' do
            context 'with JUST an ID', :smoke do
              let(:data_bag_item) {{"id" => data_bag_item_id}}
              it_behaves_like 'a successful data bag item POST'
            end

            context 'with a good ID' do
              ids = ['pedantitem', 'pedant_item', 'pedant-item', 'pedant-123-item']

              ids = ids + ['pedant:item', 'pedant.item']

              ids.each do |i|
                context "like '#{i}'" do
                  let(:data_bag_item_id){i}
                  let(:data_bag_item) {{"id" => data_bag_item_id, "answer" => 42}}
                  let(:expected_failure_response) {create_data_bag_item_invalid_id_response}
                  it_behaves_like 'a successful data bag item POST'
                end
              end
            end
          end

          context 'various bad inputs to create a data bag item', :validation do
            context 'without an ID' do
              let(:data_bag_item) {{"answer" => 42}}
              let(:expected_failure_response){create_data_bag_item_no_id_response}
              it 'really does not have an id' do
                data_bag_item.should_not have_key 'id'
              end
              it_behaves_like 'an unsuccessful data bag item POST'
            end

            context 'with a malformed ID' do
              ids = ['pedant_badId!!', '^$@^*  pedant']
              ids.each do |i|
                context "like '#{i}'" do
                  let(:data_bag_item_id){i}
                  let(:data_bag_item) {{"id" => data_bag_item_id, "answer" => 42}}
                  let(:expected_failure_response) {create_data_bag_item_invalid_id_response}
                  it_behaves_like 'an unsuccessful data bag item POST'
                end
              end
            end
          end

          respects_maximum_payload_size
        end

        # PUT isn't allowed, but that's already been tested
        context 'DELETE' do
          it 'deletes the data bag' do
            # bag is deleted
            delete(named_data_bag_url, requestor).should look_like delete_data_bag_success_response
            # it's not there anymore
            get(named_data_bag_url, requestor).should look_like data_bag_not_found_response
            # the other bag is fine
            get(api_url("/data/#{data_bag_2_name}"), requestor).should look_like ok_response
          end
        end
      end
      context 'a request to /data/<bag>/<item>' do
        let(:data_bag_name){data_bag_1_name}
        let(:data_bag_item_id){unique_name("no_item")}
        context 'GET' do
          it 'fails because there is no item' do
            get(data_bag_item_url, requestor).should look_like data_bag_item_not_found_response
          end
        end
        # POST is not allowed, but that's already been tested
        context 'PUT' do
          let(:put_body){{"id"=>data_bag_item_id}}
          it 'fails because there is no item' do
            put(data_bag_item_url, requestor, :payload => put_body).should look_like data_bag_item_not_found_from_put_response
          end
        end
        context 'DELETE' do
          it 'fails because there is no item' do
            delete(data_bag_item_url, requestor).should look_like data_bag_item_not_found_from_delete_response
          end
        end
      end # request to /data/<bag>/<item>
    end # that have no items
    context 'that have items' do
      let(:data_bag_item_1_id){unique_name("item1")}
      let(:data_bag_item_2_id){unique_name("item2")}
      let(:data_bag_item_3_id){unique_name("item3")}

      let(:data_bag_item_1){new_data_bag_item(data_bag_item_1_id)}
      let(:data_bag_item_2){new_data_bag_item(data_bag_item_2_id)}
      let(:data_bag_item_3){new_data_bag_item(data_bag_item_3_id)}

      let(:data_bag_name){data_bag_1_name}
      let(:data_bag_item_id){data_bag_item_1_id}
      let(:data_bag_item){data_bag_item_1}

      before :each do
        create_data_bag_item(admin_requestor, data_bag_name, data_bag_item_1)
        create_data_bag_item(admin_requestor, data_bag_name, data_bag_item_2)
        create_data_bag_item(admin_requestor, data_bag_name, data_bag_item_3)
      end

      after :each do
        delete_data_bag_item(admin_requestor, data_bag_name, data_bag_item_1_id)
        delete_data_bag_item(admin_requestor, data_bag_name, data_bag_item_2_id)
        delete_data_bag_item(admin_requestor, data_bag_name, data_bag_item_3_id)
      end

      context 'a request to /data' do
        context 'GET' do
          it 'returns the same non-empty list of data bags' do
            get(data_bags_url, requestor).should look_like fetch_data_bag_list_full_response
          end
        end
        # No other cases to try for POST
        # PUT is not allowed, but that's already been tested
        # DELETE is not allowed, but that's already been tested
      end # /data
      context 'a request to /data/<bag>' do
        context 'GET' do
          it 'shows a full data bag', :smoke do
            get(named_data_bag_url, requestor).should look_like fetch_full_data_bag_success_response
          end
        end
        context 'POST' do
          let(:data_bag_item){data_bag_item_1}
          it 'raises a conflict when creating an existing data bag item' do
            post(named_data_bag_url, requestor,
                 :payload => data_bag_item).should look_like create_data_bag_item_conflict_response
          end
        end
        # PUT is not allowed, but that's already been tested
        context 'DELETE', :smoke do
          it 'deletes a bag AND ALL THE ITEMS' do
            # bag is deleted
            delete(named_data_bag_url, requestor).should look_like delete_data_bag_success_response
            # it's not there anymore
            get(named_data_bag_url, requestor).should look_like data_bag_not_found_response
            # the other bag is fine
            get(api_url("/data/#{data_bag_2_name}"), requestor).should look_like ok_response
            # All the items are gone now
            [data_bag_item_1_id, data_bag_item_2_id, data_bag_item_3_id].each do |i|
              get(api_url("/data/#{data_bag_name}/#{i}"), requestor).should look_like resource_not_found_response
            end
          end
        end
      end # /data/bag
      context 'a request to /data/<bag>/<item>' do
        context 'GET' do
          let(:data_bag_item_id){data_bag_item_1_id}
          let(:data_bag_item){data_bag_item_1}
          it 'shows the complete item', :smoke do
            get(data_bag_item_url, requestor).should look_like fetch_data_bag_item_success_response
          end
        end
        # POST is not allowed, but that's already been tested
        context 'PUT' do
          let(:request_method){:PUT}
          let(:request_url){data_bag_item_url}
          context 'with various correct inputs to update a data bag item' do

            context 'with normal input', :smoke do
              let(:updated_data_bag_item) {{ "id" => data_bag_item_id, "new_stuff" => "foo" }}
              it_behaves_like "a successful data bag item PUT"
            end

            context 'with no ID in the update body' do
              let(:updated_data_bag_item) {{ "id" => data_bag_item_id, "new_stuff" => "foo" }}

              it 'successfully updates (using ID from the URL)' do
                updated_data_bag_item.delete 'id'
                updated_data_bag_item.should_not have_key 'id'

                put(data_bag_item_url, requestor,
                    :payload => updated_data_bag_item).should look_like update_data_bag_item_missing_id_response
              end
            end
          end
          context 'with various incorrect inputs to update a data bag item' do

            context 'to an item with a different id' do
              let(:different_data_bag_id) { "different" }
              let(:updated_data_bag_item) {{ "id" => different_data_bag_id, "new_stuff" => "foo" }}

              let(:expected_failure_response){update_data_bag_item_mismatched_id_response}
              it 'should have a different id' do
                updated_data_bag_item['id'].should_not eq data_bag_item_id
              end
              it_behaves_like 'an unsuccessful data bag item PUT'
            end
          end

          respects_maximum_payload_size

        end
        context 'DELETE' do
          let(:data_bag_item_id){data_bag_item_1_id}
          it 'deletes the item', :smoke do
            # delete the item
            delete(data_bag_item_url, requestor).should look_like delete_data_bag_item_success_response
            # it can't be retrieved
            get(data_bag_item_url, requestor).should look_like resource_not_found_response
            # other items are left alone
            [data_bag_item_2_id, data_bag_item_3_id].each do |i|
              get(api_url("/data/#{data_bag_name}/#{i}"), requestor).should look_like ok_response
            end
            # and the data bag itself looks sane
            get(named_data_bag_url, requestor).should look_like({
                                                                  :status => 200,
                                                                  :body_exact => {
                                                                    data_bag_item_2_id => api_url("/data/#{data_bag_name}/#{data_bag_item_2_id}"),
                                                                    data_bag_item_3_id => api_url("/data/#{data_bag_name}/#{data_bag_item_3_id}"),
                                                                  }
                                                                })
          end
        end
      end # /data/bag/item
    end # that have items
  end # with data bags
end # describe
