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

require 'pedant/rspec/node_util'
require 'pedant/rspec/internal_api'


describe "Private Chef Nodes API endpoint",  :'object-identifiers' do
  include Pedant::RSpec::NodeUtil
  include Pedant::RSpec::InternalAPI

  let(:admin_requestor){admin_user}
  let(:requestor){admin_requestor}
  let(:request_method){:GET}
  context "retrieving object identifiers" do
    let(:object_guid)    { /^[0-9A-Fa-f]{32}$/ }
    let(:request_url)    { internal_api_url("/nodes/#{node_name}/_identifiers") }

    context 'for nodes' do
      context 'that exist' do
        include_context 'with temporary testing node'
        it "returns a 200 and valid node identifiers" do
          # TODO if we expand the _identifiers behavior beyond nodes, this will
          # get factored up into its own shared context usable across different
          # object types.
          should look_like({
            :status => 200,
            :body_exact => {
              "id" => object_guid,
              "authz_id" => object_guid,
              "org_id" => object_guid
            }
          })
        end
      end

      context 'that do not exist' do
        let(:node_name) { "nonexistent_pedant_node" }
        it "returns a 404" do
          should look_like(
            { :status => 404,
              :body_exact => { "error" => ["No such 'nodes': '#{node_name}'."] } })
        end
      end
    end

    context 'for an unsupported object type' do
      let(:request_url) { internal_api_url("/search/bad_object/_identifiers") }
      it "returns a 404" do
        should look_like(
          { :status => 404,
            :body_exact => { "error" => ["Unsupported object type: 'search'."] } } )
      end
    end
  end
end
