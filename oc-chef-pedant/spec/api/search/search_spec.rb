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

#require 'pedant/rspec/client_util'
require 'pedant/rspec/data_bag_util'
require 'pedant/rspec/role_util'
require 'pedant/rspec/search_util'
require 'pedant/rspec/node_util'
require 'pedant/rspec/environment_util'
require 'pedant/rspec/client_util'
require 'pedant/rspec/chef_data'

describe 'Search API endpoint', :search do

  include Pedant::RSpec::DataBagUtil
  include Pedant::RSpec::RoleUtil
  include Pedant::RSpec::SearchUtil
  include Pedant::RSpec::NodeUtil
  include Pedant::RSpec::EnvironmentUtil
  include Pedant::RSpec::ClientUtil
  include Pedant::RSpec::ChefData

  # TODO: until we rename requestors
  shared(:admin_requestor){admin_user}
  shared(:requestor){admin_requestor}

  context '/search' do
    let(:request_url){api_url("/search")}
    context 'GET' do
      let(:request_method){:GET}
      context 'with no data bags' do
        it 'should return a list of built-in indexes' do
          should look_like fetch_search_index_success_response
        end
      end

      context 'with data bags' do
        include_context 'with testing data bag'
        let(:data_bag_names){[data_bag_name]} # allows response to incorporate data bag indexes, too
        it 'should return a list of built-in indexes, as well as indexes for each data bag' do
          should look_like fetch_search_index_success_response
        end
      end # with data bags
    end # GET
  end # /search

  context '/search/environment' do
    let(:request_url){api_url("/search/environment")}

    setup_multiple_objects :environment

    context 'GET' do
      let(:request_method){:GET}
      can_perform_basic_searches_for :environment
      can_perform_a_search_that_is_acl_filtered_for :environment
      can_perform_a_search_with_limited_rows_for :environment
    end # GET

    context 'POST'  do
      let(:request_method){:POST}

      test_bad_partial_search_bodies

      can_perform_a_partial_search_that_is_acl_filtered_for :environment

      can_perform_basic_partial_search_for(:environment,
                                           :default_attributes,
                                           {"top"=>{"middle" => {"bottom" => "found_it"}}},
                                           :attribute_path => ["top", "middle", "bottom"],
                                           :smoke => true)

      can_perform_basic_partial_search_for(:environment,
                                           :description,
                                           "behold my environment!")
    end
  end

  context '/search/node' do
    let(:request_url){api_url("/search/node")}

    setup_multiple_objects :node

    context 'GET' do
      let(:request_method){:GET}
      can_perform_basic_searches_for :node

      can_perform_a_search_that_is_acl_filtered_for :node
      can_perform_a_search_with_limited_rows_for :node

      unless Pedant.config['old_runlists_and_search']
        recipe_variants("pedant_testing_cookbook", "1.0.0").each do |run_list_recipe|
          recipe_variants("pedant_testing_cookbook", "1.0.0").each do |search_query_recipe|
            recipe_query_fragments(search_query_recipe).each do |query_frag|
              # I expect these will only work when searching for the exact form that was added to the run_list
              should_match = recipes_match?(run_list_recipe, search_query_recipe)
              node_run_list_search([run_list_recipe], query_frag, should_match, should_match ? "" : "because it is not the form that was initially entered in the run list" )
            end # query frag
          end # inner
        end # outer
      end

      # TODO: Cannot currently search for expanded run-list information via the "recipes"
      # or "roles" keys because that information is only populated on a chef-client run.

      context "with a node that has policyfile attributes" do

        let(:maximum_search_time){ Pedant::Config.maximum_search_time}

        let(:node_name) { unique_name('testing_node' ) }

        let(:nodes_container) { api_url("/nodes") }

        let(:resource_url) { api_url "/nodes/#{node_name}" }

        let(:node) do
          new_node(node_name).tap do |n|
            n["policy_name"] = "example-policy-name"
            n["policy_group"] = "example-policy-group"
          end
        end

        before do
          post(nodes_container, requestor, payload: node)
        end

        after :each do
          delete_node(admin_requestor, node_name)
        end

        it "finds nodes by policy_name" do
          search_url = request_url + "?q=policy_name:example-policy-name"
          with_search_polling do
            response = get(search_url, requestor)
            result = parse(response)
            expect(result["rows"].first).to eq(node)
          end
        end

        it "finds nodes by policy_group" do
          search_url = request_url + "?q=policy_group:example-policy-group"
          with_search_polling do
            response = get(search_url, requestor)
            result = parse(response)
            expect(result["rows"].first).to eq(node)
          end
        end

      end

    end # GET

    # Partial Search
    context 'POST' do
      let(:request_method){:POST}

      test_bad_partial_search_bodies

      can_perform_basic_partial_search_for(:node,
                                           :default,
                                           {"top"=>{"middle" => {"bottom" => "found_it"}}},
                                           :attribute_path => ["top", "middle", "bottom"],
                                           :smoke => true)

      can_perform_a_partial_search_that_is_acl_filtered_for :node

      # Node Attribute Priority Tests
      #
      # The four different types of node attributes have different priority levels, and all
      # are merged together according to those priorities when a node is indexed.  These
      # tests exhaustively cover all combinations of attribute types to verify that they are
      # indexed properly.

      # Test each type of attribute in isolation
      [:default, :normal, :override, :automatic].each do |type|
        node_attribute_partial_search("showing #{type} attributes are merged directly to the node",
                                      {
                                        type => {"testing" => {"one" => type.to_s}}
                                      },
                                      ["testing"],
                                      {"one" => type.to_s})
      end

      # Set up some stock attribute payloads.  These are structured such that all potential
      # priority effects will be apparent, regardless of which group(s) of attributes are added
      # to the node.
      default_attribute_payload = {"one" => "default", "two" => "default", "three" => "default", "four" => "default"}
      normal_attribute_payload = {"two" => "normal", "three" => "normal", "four" => "normal"}
      override_attribute_payload = {"three" => "override", "four" => "override"}
      automatic_attribute_payload = {"four" => "automatic"}

      # Default attributes overridden by higher priority attributes
      node_attribute_partial_search("showing 'default' < 'normal' attributes",
                                    {:default => {"testing" => default_attribute_payload},
                                      :normal => {"testing" => normal_attribute_payload}},
                                    ["testing"],
                                    {
                                      "one" => "default",
                                      "two" => "normal",
                                      "three" => "normal",
                                      "four" => "normal"
                                    })
      node_attribute_partial_search("showing 'default' < 'override' attributes",
                                    {:default => {"testing" => default_attribute_payload},
                                      :override => {"testing" => override_attribute_payload}},
                                    ["testing"],
                                    {
                                      "one" => "default",
                                      "two" => "default",
                                      "three" => "override",
                                      "four" => "override"
                                    })
      node_attribute_partial_search("showing 'default' < 'automatic' attributes",
                                    {:default => {"testing" => default_attribute_payload},
                                      :automatic => {"testing" => automatic_attribute_payload}},
                                    ["testing"],
                                    {
                                      "one" => "default",
                                      "two" => "default",
                                      "three" => "default",
                                      "four" => "automatic"
                                    })

      # Normal attributes overridden by higher priority attributes
      node_attribute_partial_search("showing 'normal' <  'override' attributes",
                                    {:normal => {"testing" => normal_attribute_payload},
                                      :override => {"testing" => override_attribute_payload}},
                                    ["testing"],
                                    {
                                      "two" => "normal",
                                      "three" => "override",
                                      "four" => "override"
                                    })
      node_attribute_partial_search("showing 'normal' < 'automatic' attributes",
                                    {:normal => {"testing" => normal_attribute_payload},
                                      :automatic => {"testing" => automatic_attribute_payload}},
                                    ["testing"],
                                    {
                                      "two" => "normal",
                                      "three" => "normal",
                                      "four" => "automatic"
                                    })

      # Override attributes overridden by higher priority attributes
      node_attribute_partial_search("showing 'override' <  'automatic' attributes",
                                    {:override => {"testing" => override_attribute_payload},
                                      :automatic => {"testing" => automatic_attribute_payload}},
                                    ["testing"],
                                    {
                                      "three" => "override",
                                      "four" => "automatic"
                                    })

      # Testing combinations of three kinds of attributes to verify proper "layering" behavior
      node_attribute_partial_search("showing 'default' < 'normal' < 'override' attributes",
                                    {:default => {"testing" => default_attribute_payload},
                                      :normal => {"testing" => normal_attribute_payload},
                                      :override => {"testing" => override_attribute_payload}},
                                    ["testing"],
                                    {
                                      "one" => "default",
                                      "two" => "normal",
                                      "three" => "override",
                                      "four" => "override"
                                    })

      node_attribute_partial_search("showing 'default' < 'normal' < 'automatic' attributes",
                                    {:default => {"testing" => default_attribute_payload},
                                      :normal => {"testing" => normal_attribute_payload},
                                      :automatic => {"testing" => automatic_attribute_payload}},
                                    ["testing"],
                                    {
                                      "one" => "default",
                                      "two" => "normal",
                                      "three" => "normal",
                                      "four" => "automatic"
                                    })

      node_attribute_partial_search("showing 'default' < 'override' < 'automatic' attributes",
                                    {:default => {"testing" => default_attribute_payload},
                                      :override => {"testing" => override_attribute_payload},
                                      :automatic => {"testing" => automatic_attribute_payload}},
                                    ["testing"],
                                    {
                                      "one" => "default",
                                      "two" => "default",
                                      "three" => "override",
                                      "four" => "automatic"
                                    })

      node_attribute_partial_search("showing 'normal' < 'override' < 'automatic' attributes",
                                    {:normal => {"testing" => normal_attribute_payload},
                                      :override => {"testing" => override_attribute_payload},
                                      :automatic => {"testing" => automatic_attribute_payload}},
                                    ["testing"],
                                    {
                                      "two" => "normal",
                                      "three" => "override",
                                      "four" => "automatic"
                                    })

      # Finally, try ALL types of attributes together
      node_attribute_partial_search("showing 'default' < 'normal' < 'override' < 'automatic' attributes",
                                    {:default => {"testing" => default_attribute_payload},
                                      :normal => {"testing" => normal_attribute_payload},
                                      :override => {"testing" => override_attribute_payload},
                                      :automatic => {"testing" => automatic_attribute_payload}},
                                    ["testing"],
                                    {
                                      "one" => "default",
                                      "two" => "normal",
                                      "three" => "override",
                                      "four" => "automatic"
                                    })
    end
  end # /search/node

  context '/search/role' do
    let(:request_url){api_url("/search/role")}

    setup_multiple_objects :role

    context 'GET' do
      let(:request_method){:GET}
      can_perform_basic_searches_for :role
      can_perform_a_search_that_is_acl_filtered_for :role
      can_perform_a_search_with_limited_rows_for :role
    end # GET

    # Partial Search
    context 'POST' do
      let(:request_method){:POST}

      test_bad_partial_search_bodies

      can_perform_a_partial_search_that_is_acl_filtered_for :role

      can_perform_basic_partial_search_for(:role,
                                           :override_attributes,
                                           {"top" => {"middle" => {"bottom" => "found_it"}}},
                                           :attribute_path => ["top", "middle", "bottom"],
                                           :smoke => true)

    end # POST
  end # /search/role

  context '/search/client' do
    let(:request_url){api_url("/search/client")}
    # let(:requestor){superuser}

    # Utility methods to help populate search result bodies
    def fetch_client(name)
      parse(get(api_url("/clients/#{name}"), admin_requestor))
    end

    def fetch_clients(names)
      names.map{|n| fetch_client(n)}
    end

    context 'GET' do
      let(:request_method){:GET}

      context 'with no criteria' do
        let(:query_parameters){""}
        let(:search_result_items) { fetch_clients(pedant_clients) }
        performing_a_search 'returns all the clients'
      end

      context 'searching by name', :smoke do
        let(:target_name){ admin_client.name }
        let(:request_query_parameters){"q=name:#{target_name}"}
        let(:search_result_items){ [ fetch_client(target_name) ] }
        performing_a_search 'returns the correct client'
      end # searching by name

      perform_a_search_that_returns_no_results :client
    end # GET

    context 'POST' do
      let(:request_method){:POST}

      setup_multiple_objects :client
      # Do a smoke test for a partial search for an admin client
      # Note that these tests were original OSC only - modified 'admin' to 'validator'
      # for purposes of ensuring a valid search.
      can_perform_basic_partial_search_for(:client, :validator, false, :smoke => true)

      test_bad_partial_search_bodies
    end
  end

  context '/search/<data_bag>' do
    let(:request_url){api_url("/search/#{data_bag_name}")}

    context 'using GET' do
      let(:request_method){:GET}

      context 'for a nonexistent data bag' do
        let(:data_bag_name){unique_name("no_bag")}
        let(:request_query_parameters){"q=id:no_such_item"}
        it 'should fail' do
          should look_like search_data_bag_no_data_bag_response
        end
      end

      context 'an existing data bag' do
        let(:data_bag_items){[
                              new_data_bag_item('foo'),
                              new_data_bag_item('bar'),
                             ]}
        include_context 'with testing data bag'
        include_context 'with testing data bag items' do
          let(:items){data_bag_items}
        end

        context 'a query that should succeed', :smoke do
          let(:request_query_parameters){"q=id:bar"}
          let(:search_result_items){
            # Data bag items return odd results
            [search_result_databag_item(data_bag_name, "bar", new_data_bag_item('bar'))]
          }
          performing_a_search 'should succeed'
        end # should succeed

#        perform_a_search_that_returns_no_results

        # Fix for http://tickets.chef.io/browse/CHEF-3975
        context "with nested keys (CHEF-3975)" do
          let(:items) { [ alice, bob, carol ] }
          let(:alice) { { 'id' => "alice", 'ssh' => { 'public_key' => "---RSA Public Key--- Alice", 'private_key' => "---RSA Private Key-- Alice" } } }
          let(:bob)   { { 'id' => "bob",   'ssh' => { 'public_key' => "---RSA Public Key--- Bob",   'private_key' => "---RSA Private Key-- Bob" } } }
          let(:carol) { { 'id' => "carol", 'ssh' => { 'noise' => "6b6e0824d5b85a3cd209b279bba3d5ea9df6aae891eab056521953ecb36466c8" } } }

          context "when searching a nested key", :smoke do
            let(:request_query_parameters) {"q=ssh_public_key:*"}
            let(:search_result_items) do
              [ search_result_databag_item(data_bag_name, 'alice', alice),
                search_result_databag_item(data_bag_name, 'bob',   bob) ]
            end

            performing_a_search 'should succeed'
          end

          context "when searching a nested key prefixed by raw_data_" do
            let(:request_query_parameters) {"q=raw_data_ssh_public_key:*"}
            let(:search_result_items) { [] } # We should not get anything back

            performing_a_search 'should return no results'
          end
        end

        it "should return no results to an unauthorized user", skip: !Pedant::Config.search_acls? do
          restrict_permissions_to "/data/#{data_bag_name}",
                                  normal_user => [],
                                  admin_user => ["read", "delete"]

          with_search_polling do
            r = get(api_url("/search/#{data_bag_name}?q=id:*"), normal_user)
            parse(r)["rows"].should eq([])
          end
        end

      end #existing data bag
    end # GET

    # Partial search
    context 'using POST' do
      let(:request_method){:POST}
      context 'for a nonexistent data bag' do
        it 'does something'
      end

      context 'for an existing data bag' do
        let(:data_bag_item_name){unique_name("pedant_data_bag_item")}
        include_context 'with testing data bag'
        include_context 'with testing data bag items' do
          let(:items){[new_data_bag_item(data_bag_item_name)]}
        end
        context 'a partial search', :smoke do
          let(:request_payload){ {"foo" => ["foo"]} }
          let(:search_result_items){[{
                                       "url" => api_url("/data/#{data_bag_name}/#{data_bag_item_name}"),
                                       "data" => {
                                         "foo" => "bar"
                                       }
                                     }]}
          performing_a_search "should succeed"
        end

        # Fix for http://tickets.chef.io/browse/CHEF-3975
        context "with nested keys (CHEF-3975)" do
          let(:items) { [ alice, carol ] }
          let(:alice) { { 'id' => "alice", 'ssh' => { 'public_key' => "---RSA Public Key--- Alice", 'private_key' => "---RSA Private Key-- Alice" } } }
          let(:carol) { { 'id' => "carol", 'ssh' => { 'noise' => "6b6e0824d5b85a3cd209b279bba3d5ea9df6aae891eab056521953ecb36466c8" } } }

          context "when searching a nested key", :smoke do
            let(:request_query_parameters) {"q=ssh_public_key:*"}
            let(:request_payload) { { 'private_key' => %w(ssh private_key), 'public_key' => %w(ssh public_key) } }
            let(:search_result_items) do
              [ {
                  'url'  => api_url("/data/#{data_bag_name}/#{alice['id']}"),
                  'data' => {
                    'private_key' => alice['ssh']['private_key'],
                    'public_key'  => alice['ssh']['public_key'],
                  }
              }]
            end

            performing_a_search 'should succeed'
          end

          context "when searching a nested key prefixed by raw_data_" do
            let(:request_query_parameters) {"q=raw_data_ssh_public_key:*"}
            let(:request_payload) { { 'private_key' => %w(ssh private_key), 'public_key' => %w(ssh public_key) } }
            let(:search_result_items) { [] } # We should not get anything back

            performing_a_search 'should return no results'
          end
        end

        test_bad_partial_search_bodies
      end

    end

    context 'Search tokenizer' do
      context 'When the Chef server has data bag items with "foo" and "foo-bar"' do
        data_bag 'x',
          'foo' => '{ "id": "foo" }',
          'foo-bar' => '{ "id": "foo-bar" }'

        it 'A search for foo-bar returns foo-bar and nothing else' do
            with_search_polling do
              search('x', 'id:foo-bar').map { |row| row['name'] }.should =~ [ 'data_bag_item_x_foo-bar' ]
            end
        end

        it 'A search for foo* AND NOT bar returns foo and foo-bar' do
          with_search_polling do
            search('x', 'id:foo* AND NOT bar').map { |row| row['name'] }.should =~ [ 'data_bag_item_x_foo', 'data_bag_item_x_foo-bar' ]
          end
        end
      end
    end

    context 'Special characters' do
      context 'When the Chef server has data bag items with "foo/bar"' do
        data_bag 'x',
          'foo' => '{ "id": "foo", "path": "foo/bar" }'

        it 'A search for foo/* returns foo' do
          with_search_polling do
            search('x', 'path:foo\/*').map { |row| row['name'] }.should =~ [ 'data_bag_item_x_foo' ]
          end
        end
      end
    end
  end

  # TODO: What if you try to create a data bag named 'reindex' ?

  ################################################################################
  # OLD TESTS BELOW HERE
  ################################################################################


  let(:test_data_bag) { "pedant_data_bag" }



  describe "Index Deletion" do
    context "Roles" do
      it_should_behave_like "Deletes from Solr Index" do
        let(:index_name) { "role" }
        let(:container) { "roles" }
        let(:item) { new_role(item_name) }
      end
    end

    context "Nodes" do
      it_should_behave_like "Deletes from Solr Index" do
        let(:index_name) { "node" }
        let(:container) { "nodes" }
        let(:item) { new_node(item_name) }
      end
    end

    context "Data Bag Items" do
      it_should_behave_like "Deletes from Solr Index" do

        let(:bag_name) { "test_bag" }
        let(:test_item_id) { "test_bag_item" }

        let(:item) { {"id" => test_item_id, "name" => item_name} }

        # Because data bag items are hierarchical, we need to fudge
        # things a bit to make the common tests work for them
        let(:index_name) { "data_bag_item +data_bag:#{bag_name}" }
        let(:container) { "data/#{bag_name}" } # data bags are odd
        let(:deletion_identifier) { test_item_id }

        # We also need to create and remove a containing data bag for
        # the test, too
        before :each do
          create_data_bag(admin_user, new_data_bag(bag_name) )
        end
        after :each do
          delete_data_bag(admin_user, bag_name)
        end
      end
    end

  end

  context "partial search" do

    context "roles" do
      let(:role_name) {"partial-search-role-#{rand(10000).to_s}"}
      let(:role_description) {'a role for testing partial search'}
      let(:o_attrs) { {'top' => {'mid' => {'bottom' => 'found_it'}}} }
      let(:o_attr_path) { ['override_attributes', 'top', 'mid', 'bottom'] }

      after :each do
        delete_role(admin_user, role_name)
      end

      context "many results (roles)" do
        shared(:role_names) { roles.map { |role| role['name'] }  }
        shared(:description) { "partial_search-role-description-#{rand(1000).to_s}" }
        shared(:roles) do
          (0..9).map do |i|
            new_role(a_search_item.(i), {
                       :override_attributes => {'top' => {'mid' => {'bottom' => i}}},
                       :description => description
                     })
          end
        end

        before(:all) do
          roles.each do |r|
            add_role(admin_user, r)
          end
        end

        after(:all) do
          role_names.each do |name|
            delete_role(admin_user, name)
          end
        end

        it "should have 10 results" do
          with_search_polling do
            payload = {
              'goal' => ['override_attributes', 'top', 'mid', 'bottom']
            }

            post(api_url("/search/role?q=description:#{description}"), admin_user,
                 :payload => payload) do |response|
              want_result = {
                'url' => /roles/,
                'data' => { 'goal' => 'found_it' }
              }
              # TODO this seems wrong, sine we're discarding the actual expected result...
              want_results = 10.times.map { |i| want_result }
              response.should look_like({:status => 200 })
              got = parse(response)['rows']
              got_digits = got.map { |o| o['data']['goal'] }.sort
              got_digits.should == (0..9).to_a
            end # response

          end
        end
      end

      context "nodes" do
        shared(:default_attrs) {
          {
            'top' => {'mid' => {'bottom' => 'found_it_default'}},
            'is_default' => true,
            'is' => { 'default' => true }
          }
        }

        shared(:normal_attrs) {
          {
            'top' => {'mid' => {'bottom' => 'found_it_normal'}},
            'is_normal' => true,
            'is' => { 'normal' => true }
          }
        }

        shared(:node_name) { a_search_item.('node') }

        shared(:a_node) {
          n = new_node(node_name)
          n['default'] = default_attrs
          n['normal'] = normal_attrs
          n
        }

        before(:all) do
          add_node(admin_user, a_node)
        end

        after(:all) do
          delete_node(admin_user, node_name)
        end

        it "returns partial results from default attributes of a node" do
          with_search_polling do
            payload = {
              'we_found_default' => ['is', 'default'],
            }
            post(api_url("/search/node?q=name:#{node_name}"), admin_user,
                 :payload => payload) do |response|
              response.should look_like({:status => 200,
                                          :body => {
                                           "rows" => [{ 'url' => api_url("/nodes/#{node_name}"),
                                                        'data' => {
                                                          'we_found_default' => true
                                                        }}]}})
            end
          end
        end

        it "returns partial results from normal attributes of a node" do
          with_search_polling do
            payload = {
              'we_found_normal' => ['is', 'normal'],
            }
            post(api_url("/search/node?q=name:#{node_name}"), admin_user,
                 :payload => payload) do |response|
              response.should look_like({:status => 200,
                                         :body => {
                                           "rows" => [{ 'url' => api_url("/nodes/#{node_name}"),
                                                        'data' => {
                                                          'we_found_normal' => true
                                                        }}]}})
            end
          end
        end

        it "returns partial results from deep merged node attributes" do
          with_search_polling do
            payload = {
              'goal' => ['top', 'mid', 'bottom'],
            }
            post(api_url("/search/node?q=name:#{node_name}"), admin_user,
                 :payload => payload) do |response|
              response.should look_like({:status => 200,
                                         :body => {
                                           "rows" => [{ 'url' => api_url("/nodes/#{node_name}"),
                                                        'data' => {
                                                          'goal' => 'found_it_normal'
                                                        }}]}})
            end
          end
        end
      end # context
    end
  end
end
