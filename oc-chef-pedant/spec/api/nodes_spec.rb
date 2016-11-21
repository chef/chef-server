#
# -*- indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
# ex: ts=4 sw=4 et
#
# Author:: Mark Anderson (<mark@chef.io>)
# Author:: Christopher Maier (<cm@chef.io>)
# Copyright:: Copyright (c) 2011 Opscode, Inc.
#

# Considering nodes endpoint to only be implemented in Erlang, since
# the Ruby version still assumes things are stored in CouchDB, instead
# of MySQL

require 'pedant/rspec/node_util'

describe "Private Chef Nodes API endpoint", :nodes do
  include Pedant::RSpec::NodeUtil


  context "updating a node" do
    ## TODO: Consider pulling up to node util
    let(:nodes_url) { api_url("/nodes") }
    let(:named_node_url) { api_url("/nodes/#{node_name}") }

    let(:node_name) { "putter" }
    let(:requestor) { admin_user }
    let(:node) { new_node(node_name) }

    before :each do
      add_node(admin_user, node)
    end

    after :each do
      delete_node(admin_user, node_name)
    end

    context 'attempting to change node name on update', :validation do
      let(:node_update_name_mismatch_response) {
        {
          :status => 400,
          :body => {
            "error" => ["Node name mismatch."]
          }
        }
      }

      let(:invalid_node) {
        {
          "name" => "Not_the_same_name",
          "json_class" => "Chef::Node"
        }
      }

      let(:expected_response){ node_update_name_mismatch_response }

      it 'returns a 400 ("Bad Request")' do
        invalid_node["name"].should_not == node_name

        put(named_node_url, requestor, :payload => invalid_node) do |response|
          response.should look_like expected_response
        end
      end
    end
  end
end
