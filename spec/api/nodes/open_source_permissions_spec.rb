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

require 'pedant/rspec/node_util'
require 'pedant/opensource/permission_checks'

describe 'Node Endpoint Open Source Permission Checks', :platform => :open_source, :nodes => true do
  include Pedant::RSpec::NodeUtil
  include Pedant::OpenSource::PermissionChecks

  # Until we rename the requestors
  let(:admin_requestor){admin_user}

  context '/nodes' do
    let(:request_url){api_url('/nodes')}
    context 'GET' do
      let(:request_method){:GET}
      include_context 'permission checks' do
        let(:admin_response){ok_response}
        let(:non_admin_response){ok_response}
      end
    end
    context 'POST' do
      let(:request_method){:POST}
      let(:node_name){"testing_node"}
      let(:request_payload){ new_node(node_name)}

      after :each do
        delete_node(admin_requestor, node_name)
      end

      include_context 'permission checks' do
        let(:admin_response){created_response}
        # Non-admins can create a node apparently due to node bootstrapping
        let(:non_admin_response){created_response}
      end
    end

    should_not_allow_method :PUT
    should_not_allow_method :DELETE

  end # /nodes

  context '/nodes/<node>' do
    include_context 'with temporary testing node'
    let(:request_url){api_url("/nodes/#{node_name}")}

    context 'GET' do
      let(:request_method){:GET}
      include_context 'permission checks' do
        let(:admin_response){ok_response}
        let(:non_admin_response){ok_response}
      end
    end

    should_not_allow_method :POST

    context 'PUT' do
      let(:request_method){:PUT}
      let(:minimal_node_update) do
        {
          "json_class" => "Chef::Node",
          "run_list" => []
        }
      end

      let(:request_payload){minimal_node_update}
      include_context 'permission checks' do
        let(:admin_response){ok_response}
        let(:non_admin_response){forbidden_response}
      end
    end
    context 'DELETE' do
      let(:request_method){:DELETE}
      include_context 'permission checks' do
        let(:admin_response){ok_response}
        let(:non_admin_response){forbidden_response}
      end
    end
  end # /nodes/<node>
end
