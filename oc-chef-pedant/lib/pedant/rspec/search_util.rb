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

require 'pedant/request'
require 'rspec/core/shared_context'
require 'pedant/concern'
require 'uri'

module Pedant
  module RSpec
    module SearchUtil
      extend ::RSpec::Core::SharedContext
      extend Pedant::Concern

      included do

        shared(:a_search_item) { ->(x) { "pedant-search-target-#{x}-#{unique_suffix}" } }
      end

      module ClassMethods

        # Given a cookbook name and a version string (e.g., "1.0.0"), generate the legal
        # variants of how the recipe can be represented in a run list
        #
        # NOTE: currently, searching for recipes with @ in the name
        # appears to be buggy.  When that has been fixed, uncomment
        # the commented-out variants below to enable tests for that.
        def recipe_variants(cookbook_name, version)
          [
           cookbook_name,
           "#{cookbook_name}::default",
           "#{cookbook_name}::default@#{version}",
           "#{cookbook_name}@#{version}",
           "recipe[#{cookbook_name}]",
           "recipe[#{cookbook_name}::default]",
           "recipe[#{cookbook_name}::default@#{version}]",
           "recipe[#{cookbook_name}@#{version}]"
          ]
        end

        # For a given recipe, generate the (unescaped) Chef search
        # query fragments that could be used to retrieve the node with
        # that recipe in its run list
        def recipe_query_fragments(recipe_name)
          stripped_name = strip_recipe(recipe_name)
          [
           "recipe:#{stripped_name}",
           "run_list:recipe[#{stripped_name}]"
          ]
        end

        # Given a recipe name, return it stripped of any "recipe[...]"
        # wrapping
        def strip_recipe(recipe_name)
          case recipe_name
          when /^recipe\[/
            recipe_name.gsub(/^recipe\[(.*)\]/, '\1')
          else
            # bare recipe; return unchaged
            recipe_name
          end
        end

        # Returns true if two recipes match exactly, modulo any
        # "recipe[...]" qualification.  This is textual matching, NOT
        # semantic matching; "foo" is not the same as "foo::default",
        # even though they mean the same thing to Chef (currently the
        # server recognizes these as distinct for the purposes of
        # storage and search)
        def recipes_match?(r1, r2)
          strip_recipe(r1) == strip_recipe(r2)
        end

        def performing_a_search(message)
          it message do
            with_search_polling do
              # This is the same logic used by the
              # 'response' RSpec implicit subject
              # that we use everywhere else.  We
              # can't use that, though, because
              # (as a `let` block) the value is
              # memoized, which works against us
              # in this particular scenario
              #
              # The fact that the parameters to
              # authenticated_request are memoized
              # doesn't affect us, though.
              r = authenticated_request(request_method, request_url_with_query_parameters, requestor, request_options)
              r.should look_like search_success_response
            end
          end
        end

        def can_perform_a_search_that_is_acl_filtered_for(object_type)
          valid_object_type?(object_type)
          it "should return filtered results when ACL on #{object_type}s exist", skip: !Pedant::Config.search_acls? do
            restrict_permissions_to "/#{object_type}s/#{base_object_name}_3",
                                    normal_user => ["delete"],
                                    admin_user => ["read"]

            # A little bit of confirmation that the ACL has applied correctly
            n = get(api_url("/#{object_type}s/#{base_object_name}_3"), normal_user)
            n.should look_like({:status => 403})

            with_search_polling do
              admin_response = get("#{request_url}/?q=name:*", admin_user)
              parse(admin_response)["rows"].any? {|row| row["name"] == "#{base_object_name}_3"}.should be true
              r = get("#{request_url}/?q=name:*", normal_user)
              parse(r)["rows"].any? {|row| row["name"] == "#{base_object_name}_3"}.should be false
            end
          end
        end

        def perform_a_search_that_returns_no_results(object_type)
          valid_object_type?(object_type)
          context 'a search that should return no results' do
            let(:request_query_parameters){"q=no_way:no_how"}
            let(:search_result_items){[]}

            it "should have multiple #{object_type}s on the system (for our search to ignore)" do
              r = get(api_url("/#{object_type}s"), requestor)
              r.should have_status_code 200 # sanity check
              parsed_body = parse(r)

              # Verifies there are other things that could potentially
              # be returned from a misbehaving search... there should
              # be at least the multiple things we created via
              # `setup_multiple_objects`, plus the one thing we're
              # looking for.
              parsed_body.keys.length.should be > 0
            end

            performing_a_search 'returns 200, with no search results'
          end
        end

        def test_bad_partial_search_bodies
          context "invalid partial search requests" do
            bad_payloads = [
                            "z[$blah",
                            {'a_string' => 'blah'},
                            {'a_number' => 1},
                            {'a_true' => true},
                            {'an_object' => {'oop' => true}},
                            {'an_array' => [1, 2]},
                            {'an_array' => ['a', 2]}
                           ]

            bad_payloads.each do |bad|
              context "with a request body of '#{bad}'", :validation do
                let(:request_query_parameters){"q=name:make-no-difference"}
                let(:request_payload){bad}
                it "fails" do
                  should look_like({:status => 400}) #bad_request_response
                end # it fails
              end # with a request body of
            end # each
          end  # invalid partial search
        end # test_bad_partial_search_bodies

        # Simple helper method to ensure we're using the correct Chef
        # object type.  Currently only used here in search utility
        # methods, but could conceivably be used elsewhere in the
        # future.
        def valid_object_type?(object_type)
          unless ["environment", "role", "node", "client"].include?(object_type.to_s)
            raise "#{object_type} is not a valid object type!"
          end
        end

        # Helper macro to set up multiple objects.  Mainly of use for
        # generating enough objects for search testing.
        #
        # Currently these are 'before :each' instead of 'before :all'
        # setups.  Unfortunately, this means that test times will be
        # extended.  This is necessary because of bugs in RSpec around
        # `before :all` and `let` blocks.  Apparently, using `shared`
        # doesn't alleviate the problem, either.
        #
        # Currently, the objects that are created differ only in their
        # name.  As a result, they are mainly useful in providing a
        # "background" against which searches can be performed, to
        # demonstrate the selectivity of the search function.
        #
        # Exposes the following `let` blocks for use within whatever
        # context this is called.
        #
        # base_object_name => All objects will have a name that starts
        # with this prefix, and will have a numerical suffix between 1
        # and `number_to_create`
        #
        # object_names => An array of the names of all objects created
        #
        # objects => An array of all the objects created
        def setup_multiple_objects(object_type, number_to_create=5)
          # Basic sanity check
          valid_object_type?(object_type)

          # Make use of existing helper functions to create new
          # objects
          #
          # To aid grep-ability for the use of these functions, we'll
          # add the full names here:
          #
          #     new_environment, new_role, new_node, new_client
          #     add_environment, add_role, add_node, add_client
          #     delete_environment, delete_role, delete_node, delete_client
          object_creator_method_symbol = "new_#{object_type}".to_sym
          object_add_method_symbol = "add_#{object_type}".to_sym
          object_delete_method_symbol = "delete_#{object_type}".to_sym

          let(:base_object_name){unique_name("multiple_#{object_type}")}
          let(:object_names){(1..number_to_create).map{|n| "#{base_object_name}_#{n}"}}
          # TODO: come up with a way to pass in name/body pairs for more flexibility?
          let(:objects){object_names.map{|n| send(object_creator_method_symbol, n)}}

          before :each do
            objects.each do |o|
              send(object_add_method_symbol, admin_requestor, o)
            end
          end

          after :each do
            objects.each do |o|
              send(object_delete_method_symbol, admin_requestor, o['name'])
            end
          end
        end

        # These are some basic search cases (i.e. using GET, not POST
        # for partial search) that are common to many object searches.
        # To work properly, this method MUST be called in a context
        # where `setup_multiple_objects` has been called, as it relies
        # on the `let` blocks that method generates.  Furthermore, it
        # is assumed (but not verified) that the `request_method`
        # block for the enclosing context is set to `:GET` and that
        # the `request_url` block has the appropriate value for the
        # search index you wish to query.
        #
        # These are not exhaustive search tests; indeed, many object
        # searches will require additional, object-specific tests to
        # fully exercise the search endpoints.  These do provide basic
        # coverage of the core use cases, though.
        def can_perform_basic_searches_for(object_type)
          valid_object_type?(object_type)

          # This is the object we're going to be searching for, out of
          # the background of all the others generated by
          # `setup_multiple_objects`
          include_context "with temporary testing #{object_type}"

          # The 'with temporary testing XXX' context exposes a let
          # block named "XXX_name" that contains the name of the
          # object it created.
          name_block_symbol = "#{object_type}_name".to_sym

          # Similarly, it exposes a block that contains the entire
          # object that was created
          object_block_symbol = object_type.to_sym

          perform_a_search_that_returns_no_results(object_type)

          context "when searching for a single #{object_type} by name", :smoke do
            let(:search_target_name){send(name_block_symbol)}
            let(:request_query_parameters){"q=name:#{search_target_name}"}
            let(:search_result_items){[send(object_block_symbol)]}

            # This is just a sanity check
            it "should have more than just the target of our #{object_type} search on the system" do
              r = get(api_url("/#{object_type}s"), requestor)
              r.should have_status_code 200 # sanity check
              parsed_body = parse(r)

              # Verifies that our search target is there
              parsed_body.should have_key search_target_name

              # Verifies there are other things that could potentially
              # be returned from a misbehaving search... there should
              # be at least the multiple things we created via
              # `setup_multiple_objects`, plus the one thing we're
              # looking for.
              parsed_body.keys.length.should be >= (1 + objects.length)
            end

            performing_a_search "should return status code 200 and a single #{object_type}"
          end

          context "when searching for multiple #{object_type}s names using a * wildcard" do
            # This test retrieves all the objects generated by
            # `setup_multiple_objects`, but should not return the
            # single object created by the 'with temporary testing
            # XXX' context.
            let(:request_query_parameters){"q=name:#{base_object_name}*"} # <- note the '*'
            let(:search_result_items){objects}

            # Another sanity check
            it "should have more than just the targets of our #{object_type} search on the system" do
              r = get(api_url("/#{object_type}s"), requestor)
              r.should have_status_code 200 # sanity check
              parsed_body = parse(r)

              # Verifies there are other things that could potentially
              # be returned from a misbehaving search... there should
              # be at least the multiple things we created via
              # `setup_multiple_objects`, plus the one thing we're
              # looking for.
              parsed_body.keys.length.should be > objects.length
            end

            performing_a_search "should return all testing #{object_type}s"
          end
        end # can_perform_basic_searches_for

        # Creates an object of the given type, with `attribute_value`
        # stored under `attribute_key`.  Performs a partial search to
        # pull out the item located at `attribute_path` *INSIDE*
        # `attribute_value`.
        #
        # Performs several partial searches, targeting the searches
        # via query parameters to 0, 1, and >1 objects.
        #
        # Should be invoked in a context that has `request_method` set
        # to :POST, and `request_url` set to the appropriate search
        # index.  Also, `setup_multiple_objects` must have been called
        # as well, since these tests depend on the `let` blocks that
        # generates.
        #
        # `attribute_key` is the field of the object whose value
        # should be set to `attribute_value`.  `attribute_path` is an
        # array of keys into `attribute_value` (if `attribute_value`
        # is a hash, that is; if it's not, don't set a value for
        # `attribute_path`); the value at the end of the path in the
        # hash will be returned in the partial search results.  It is
        # important that you do NOT have the first element of
        # `attribute_path` equal to `attribute_key`; the path you
        # provide should be relative to the `attribute_value` you
        # supply, NOT relative to the overall object itself.
        #
        # If `smoke` is true, then the "basic" partial search test
        # generated by this macro will be tagged as a smoke test.  By
        # default, no tests are tagged as such.
        def can_perform_basic_partial_search_for(object_type, attribute_key, attribute_value, options={})

          # Pull out optional parameters
          attribute_path = options[:attribute_path] || []
          smoke = options[:smoke] || false


          valid_object_type?(object_type)

          # In order to target a single object, we will create one
          # here.  To target multiple objects, we rely on
          # `setup_multiple_objects`, which we assume has already been
          # called.
          include_context "with temporary testing #{object_type}" do
            # All 'with temporary testing XXX' contexts allow you to
            # override the value of any object attribute by passing in
            # a `let` block named `XXX_ATTRIBUTE`
            let("#{object_type}_#{attribute_key}".to_sym) {attribute_value}
          end

          # This will be used to query the name of the object just
          # generated by the 'with temporary testing XXX' context (all
          # such contexts create a `let` block named 'XXX_name')
          object_name_symbol = "#{object_type}_name".to_sym


          # Declaring this a a plain old variable so we can use it in
          # context descriptions later
          partial_search_payload = {
              # This will pull out the nested  attribute we set earlier
              "possibly_nested" => real_search_path(object_type, attribute_key, attribute_path),
              "the_name" => ["name"],
              # there is no data in the object corresponding to this path
              "not_found" => ["foo", "bar", "baz", "totally_not_a_real_field"],
              "empty" => []
            }
          let(:request_payload){partial_search_payload}

          # When we do a partial search targeted toward a single
          # object, this is what we expect to get back.
          let(:single_search_expected_results) do
            [{
               "url" => api_url("/#{object_type}s/#{send(object_name_symbol)}"),
               "data" => {
                  # This is getting the value from the JSON as posted
                  # to create the object, which is not necessarily the
                  # same as the data that is ultimately indexed (like
                  # nodes)
                 "possibly_nested"=> possibly_nested_target((send object_type.to_sym),
                                                            attribute_key, attribute_path),
                 "the_name" => send(object_name_symbol),
                 "not_found" => nil,
                 "empty" => nil
               }
             }]
          end

          # We'll also run a search targeted at ALL the objects that
          # Pedant has generated (for this test, anyway).  This relies
          # on the `let` blocks generated by invoking
          # `setup_multiple_objects`.  It appends additional response
          # item bodies onto `single_search_expected_results`
          let(:multiple_search_expected_results) do
            single_search_expected_results +
            objects.map{|o|
              {
                "url" => api_url("/#{object_type}s/#{o['name']}"),
                "data" => {
                  # This is getting the value from the JSON as posted
                  # to create the object, which is not necessarily the
                  # same as the data that is ultimately indexed (like
                  # nodes)
                  "possibly_nested"=> possibly_nested_target(o, attribute_key, attribute_path),
                  "the_name" => o['name'],
                  "not_found" => nil,
                  "empty" => nil
                }
              }
            }
          end

          context "targeted toward no #{object_type}s with body of #{partial_search_payload}" do
            let(:request_query_parameters){"q=no_way:no_how"} # This isn't going to find anything
            let(:search_result_items){ [] }
            performing_a_search 'should succeed, but return nothing'
          end

          context "targeted toward one #{object_type} with body of #{partial_search_payload}" do
            # Isolate query to the object we just created

            let(:request_query_parameters){"q=name:#{send(object_name_symbol)}"}
            let(:search_result_items){single_search_expected_results}
            performing_a_search "should succeed, and return the single #{object_type}"
          end

          context "targeted toward many #{object_type}s with body of #{partial_search_payload}", :smoke => smoke do
            # Isolate the query to our test objects + our custom created one, ignoring anything that Pedant did not make
            let(:request_query_parameters){"q=name:#{send(object_name_symbol)}%20OR%20name:#{base_object_name}*"}
            let(:search_result_items){multiple_search_expected_results}
            performing_a_search "should succeed, and return multiple #{object_type}s"
          end
        end # can_perform_basic_partial_search_for

        def can_perform_a_partial_search_that_is_acl_filtered_for(object_type)
          valid_object_type? object_type
          it "should return filtered results when ACLs exist", skip: !Pedant::Config.search_acls? do
            restrict_permissions_to "/#{object_type}s/#{base_object_name}_3",
                                    normal_user => ["delete"],
                                    admin_user => ["read"]

            payload = { "name" => ["name"] }
            with_search_polling do
              admin_response = post("#{request_url}?q=name:*", admin_user, {payload: payload})
              parse(admin_response)["rows"].any? {|row| row["data"]["name"] == "#{base_object_name}_3"}.should be true
              r = post("#{request_url}?q=name:*", normal_user, {payload: payload})
              parse(r)["rows"].any? {|row| row["data"]["name"] == "#{base_object_name}_3"}.should be false
            end
          end
        end

        # Helper method to determine the "real" search path that
        # should be submitted in the partial search request body.
        # Attributes of nodes are merged together and then added
        # directly to the node body itself; the various types of
        # attributes are not "preserved" for the indexed data.  As
        # such, those keys should not be present at the beginning of
        # the search path.
        def real_search_path(object_type, key, sub_path)
          if (object_type.to_s == 'node') &&
              (['default', 'normal', 'override', 'automatic'].include? key.to_s)
            sub_path
          else
            [key.to_s] + sub_path
          end
        end

        # Exercise node attribute overrides via partial search. In
        # principle, there's no reason this necessarily needs to be a
        # partial search, but it further exercises that functionality,
        # and it's easy to check.
        #
        # Must be executed in a context where `request_method` is set
        # to :POST and `request_url` is set to api_url("/search/node")
        #
        # `message` is a descriptive string, which will be
        # incorporated into the test example's description
        #
        # `attribute_hash` is a hash with some subset of :default,
        # :normal, :override, and :automatic keys; the values of these
        # will, if present, be added to a testing node created for
        # this test.
        #
        # `search_path` is an array of string keys into the (indexed)
        # node for extracting information via a partial search.
        #
        # `expected_result` is what a partial search should bring back
        # from the node, given `search_path`.  This should just be the
        # raw data, and should not have the "partial search envelope"
        # around it; the test adds that for you.
        def node_attribute_partial_search(message, attribute_hash, search_path, expected_result)
          default = attribute_hash[:default] || {}
          normal = attribute_hash[:normal] || {}
          override = attribute_hash[:override] || {}
          automatic = attribute_hash[:automatic] || {}

          context "searching a node with #{attribute_hash.keys} attributes, with a partial search path of #{search_path}" do
            include_context 'with temporary testing node' do
              let(:node_default){default}
              let(:node_normal){normal}
              let(:node_override){override}
              let(:node_automatic){automatic}
            end

            # Just target the search to this node; we're testing the
            # override properties of attributes for the indexing
            # process
            let(:request_query_parameters){"q=name:#{node_name}"}
            let(:request_payload) do
              {"target" => search_path}
            end
            let(:search_result_items) do
              [{
                 "url" => api_url("/nodes/#{node_name}"),
                 "data" => {
                   "target" => expected_result
                 }
               }]
            end
            performing_a_search "should return #{expected_result}, #{message}"
          end
        end # node_attribute_partial_search

        # Normal search, needs to be run with method=GET
        def node_run_list_search(run_list, search_query_fragment, should_find=true, message=nil)
          context "with a run_list of #{run_list}" do

            include_context 'with temporary testing node' do
              let(:node_run_list){run_list}
            end

            let(:request_query_parameters){"q=#{sanitize_query_fragment(search_query_fragment)}"}
            let(:search_result_items) {should_find ? [normalize_node(node)] : []}

            performing_a_search "searching for #{search_query_fragment} (properly escaped) should#{should_find ? ' ' : " not "}return the node#{message ? ', ' + message : ''}"

          end
        end # with a run list
      end # ClassMethods

      # Replace :, [, ], and @ with escaped, URL-encoded versions of
      # same.  This makes writing the tests easier because you don't
      # have to keep track of the encoding yourself.
      #
      # Not doing global URL encoding; just the Solr query operators
      def sanitize_query_fragment(search_query_fragment)
        search_query_fragment.gsub(/(?<!^recipe|^role|^run_list):/, '%5C:').gsub(/[\[\]]/, '[' => '%5C%5B', ']' => '%5C%5D').gsub(/@/, '@' =>'%40')
      end

      # Given a JSON representation of a Chef object (e.g., as
      # submitted via a POST request), a key/field within that object,
      # and a path of keys (possibly empty) into the value of the
      # object at that field, return the item found at the end of that
      # path.
      #
      # Note that this depends on original_key and sub_path having the
      # same relationship as the attribute_key and attribute_path do
      # for can_perform_basic_partial_search_for.  Currently this method is only
      # used in that macro.
      def possibly_nested_target(object, original_key, sub_path)
        ([original_key.to_s] + sub_path).reduce(object){|obj, elem|
          lookup = obj[elem]
          return if lookup.nil? # the value could be false!!
          lookup
        }
      end

      built_in_indexes = ['client', 'environment', 'node', 'role']

      let(:fetch_search_index_success_response) do
        dbs = begin data_bag_names rescue [] end
        index_names = built_in_indexes + dbs

        {
          :status => 200,
          :body_exact => index_names.inject({}){|acc, i|
            acc[i] = api_url("/search/#{i}")
            acc
          }
        }
      end

      let(:reindex_search_success_response) do
        {
          :status => 200,
          :body_exact => {
            "Chef::ApiClient" =>"success",
            "Chef::Node" => "success",
            "Chef::Role" => "success",
            "Chef::Environment" =>"success",
            "Chef::DataBag" => "success"
          }
        }
      end

      # This can be used for GETs as well as POSTs
      # (i.e., 'normal' and partial searches)
      #
      # Relies on 'search_result_items' being set to an array of
      # whatever items you expect the search to return (e.g., the
      # nodes if you're searching nodes, the partial response mappings
      # if you're doing a partial search, etc.)
      #
      # The actual order of `search_result_items` is unimportant, as
      # the `look_like` matcher will make sure that each item is
      # present in the results.
      let(:search_success_response) do
        {
          :status => 200,
          :body => {
            "start" => 0, # TODO: Test paging
            "rows" => search_result_items
          }
        }
      end

      require 'uri'
      require 'cgi'

      # Amount of time to try searches until giving up... this gives Solr
      # an opportunity to commit.
      let(:maximum_search_time){ Pedant::Config.maximum_search_time}
      # Databag items that come back from a search are wrapped in a bit of
      # extra cruft.
      #
      # `item` should be a Ruby hash with String keys
      def search_result_databag_item(bag_name, item_name, item)
        {
          "name" => "data_bag_item_#{bag_name}_#{item_name}",
          "json_class" => "Chef::DataBagItem",
          "chef_type" => "data_bag_item",
          "data_bag" => bag_name,
          "raw_data" => item
        }
      end

      # Due to how we do indexing, the search results coming back from
      # Erchef are not quite what is in Solr.  As a result, we need to
      # actually hit Solr directly in order to verify that e.g., we
      # actually delete things from the index
      #
      # Returns the JSON body, converted to a Ruby hash.
      def direct_solr_query(type, query)
        # NOTE: in production, I believe we use 'fq' for filtering on the
        # Chef Object type (in addition to filtering based on org).  We
        # don't have an easy way to access the org's guid in the tests, so
        # I'm not using that. In any event, the following query works.
        url = "#{Pedant::Config.search_server}#{Pedant::Config.search_url_fmt}" % {:type => CGI.escape(type), :query => CGI.escape(query)}
        headers = {
          "Accept" => "application/json"
        }
        sleep Pedant::Config.direct_solr_query_sleep_time
        r = RestClient.send :get, url, headers
        parse(r)
      end

      def get_response_count(r)
        if r["response"].nil?
          r["hits"]["total"]
        else
          r["response"]["numFound"]
        end
      end

      # Force a commit on Solr.  Call this after adding things that you
      # subsequently want to search for.  Much preferable to waiting
      # around for a minute.
      def force_solr_commit
        # assuming we're running this after adding
        # some things to Solr, we want to give it a little
        # time to clear the queue.  In a test scenario, this
        # should be enough of a wait.
        sleep Pedant::Config.direct_solr_query_sleep_time
        url = "#{Pedant::Config.search_server}#{Pedant::Config.search_commit_url}"
        body = ''
        headers = {}
        RestClient.send :post, url, body, headers
      end

      # Intelligently execute search requests, taking into account the lag
      # for Solr commits.  By default, this method retries the block until
      # it runs without exception, up to a specified threshold.  If a
      # `search_server` parameter is specified in the Pedant config file,
      # however, a commit packet is sent to that server and the block is
      # executed only once.
      def with_search_polling(time=maximum_search_time, increment = 5, elapsed_time = 0, &block)
        if Pedant::Config.search_server
          force_solr_commit
          yield
        else
          begin
            yield
          rescue Exception => e
            if elapsed_time >= time
              raise e
            else
              sleep increment
              with_search_polling(time, increment, elapsed_time + increment, &block)
            end
          end
        end
      end

      def search_should_return(options)
        user = options[:user] || admin_user
        results = options[:results]
        with_search_polling do
          get(api_url("/search/#{options[:type]}?q=#{options[:query]}"),user) do |response|
            response.should look_like({
                                        :status => 200,
                                        :body => {
                                          "start" => 0,
                                          "rows" => results
                                        }
                                      })
          end
        end
      end

      def search_result(index, query)
        search_url = api_url(URI::encode("/search/#{index}?q=#{query}"))
        get(search_url, admin_user)
      end

      def search(index, query)
        with_search_polling do
          response = search_result(index, query)
          response.should look_like({:status => 200, :body => { 'start' => 0 }})
          parse(response)['rows']
        end
      end
    end

    shared_examples "Deletes from Solr Index" do
      if Pedant::Config.search_server

        let(:requestor) { admin_user }
        # A string prefix, followed by the number of seconds from the
        # epoch, followed by the number of nanoseconds from the last
        # fractional second from the epoch, followed by the pid.  This
        # ought to be suitably unique.  Grabbing a UUID gem just for
        # this seems a bit overblown.
        let(:item_name) { a_search_item.('resource') }
        let(:deletion_identifier) { item_name } #default to item_name; override this for data bag items

        after :each do
          delete_chef_object(container, requestor, item_name)
        end

        let(:index_name) { raise 'Must specify an :index_name! (e.g., a search index like "node", "role", or "data")' }
        let(:container) { raise 'Must specify a :container! (e.g., "nodes", "roles", "data", etc.' }
        let(:item) { raise 'Must specify an :item (JSON string or Hash to insert)'}

        it "deletes an object from Solr when deleting from the system as a whole" do
          # Assert that there is no item of the given type with the given name in the search index
          r = direct_solr_query(index_name, "content:name__=__#{item_name}")
          num_before_add = get_response_count(r)
          num_before_add.should eq 0

          # Now add the item and force a Solr commit
          add_chef_object(container, requestor, item)
          force_solr_commit

          # Verify that it is now searchable
          r2 = direct_solr_query(index_name, "content:name__=__#{item_name}")
          num_after_add = get_response_count(r2)
          num_after_add.should eq 1
          force_solr_commit

          # Now delete the object and force a Solr commit
          delete_chef_object(container, requestor, deletion_identifier)
          force_solr_commit

          # Searching for the object should retrieve no results from Solr
          r3 = direct_solr_query(index_name, "content:name__=__#{item_name}")
          num_after_delete = get_response_count(r3)
          num_after_delete.should eq 0
        end
      end

    end

    shared_examples "Reindexing" do
      include_context "with temporary testing node"
      include_context "with temporary testing role"
      include_context "with temporary testing environment"
      # client can just use validator to test; don't bother making a new one
      include_context "with testing data bag"
      include_context "with testing data bag items" do
        let(:items) {
          array = [{'id' => 'test_item', 'key' => 'value'}]
          # add a bunch of junk so reindex will scroll
          for i in 0..1010
            array << {'id' => "test_item#{i}", 'key' => 'value'}
          end
          array
        }
      end

      # Arguments supplied to the reindexing escript after the subcommand.
      let(:reindex_args){[]}

      def should_find(type, name)
        do_search(type, name, true)
      end

      def should_not_find(type, name)
        do_search(type, name, false)
      end

      def do_search(type, name, should_find=true)
        with_search_polling do
          result = authenticated_request(:GET, api_url("/search/#{type}"), requestor, {})
          result.should have_status_code 200
          identifiers = case type
                        when "node", "role", "environment"
                          parse(result)["rows"].map{|r| r['name']}
                        when "client"
                          parse(result)["rows"].map{|r| r['name'] || r['clientname']}
                        else # data bag
                          parse(result)["rows"].map{|r| r['raw_data']['id']}
                        end
          if should_find
            identifiers.should include(name)
          else
            identifiers.should_not include(name)
          end
        end
      end

      it "works for all object types" do
        # Ensure that a search against each Chef object type is
        # successful BEFORE any reindexing operations.
        should_find("node", node_name)
        should_find("role", role_name)
        should_find("environment", environment_name)
        should_find("client", admin_client.name)
        should_find(temporary_data_bag_name, "test_item")

        # Now, drop all information from the search index
        `#{executable} drop #{reindex_args.join(" ")} #{Pedant::Config.reindex_endpoint}`

        # Verify that searches come up empty
        should_not_find("node", node_name)
        should_not_find("role", role_name)
        should_not_find("environment", environment_name)
        should_not_find("client", admin_client.name)
        should_not_find(temporary_data_bag_name, "test_item")

        # Now, send everything to be re-indexed
        `#{executable} reindex #{reindex_args.join(" ")} #{Pedant::Config.reindex_endpoint}`

        # Verify that the reindexing worked by finding all the items
        # again.  Remember, there are implicit Solr commit calls being
        # made here; it'd take a bit longer for these to succeed
        # otherwise.
        should_find("node", node_name)
        should_find("role", role_name)
        should_find("environment", environment_name)
        should_find("client", admin_client.name)
        should_find(temporary_data_bag_name, "test_item")
      end
    end # reindexing test

  end # RSpec
end # Pedant
