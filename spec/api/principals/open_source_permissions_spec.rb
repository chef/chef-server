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

require 'pedant/rspec/principal_util'
require 'pedant/opensource/permission_checks'

describe 'Principal Endpoint Open Source Permission Checks', :principals => true, :platform => :open_source do
  include Pedant::RSpec::PrincipalUtil
  include Pedant::OpenSource::PermissionChecks

  context "/principals" do
    # This endpoint doesn't exist yet.

    #let(:request_url){ api_url("/principals") }

    context "GET" do
      #let(:request_method){:GET}
      #include_context 'permission checks' do
        #let(:admin_response){ok_response}
        #let(:non_admin_response){ok_response}
        #let(:validator_response){ok_response}
      #end
    end

    #should_not_allow_method :POST
    #should_not_allow_method :PUT
    #should_not_allow_method :DELETE
  end # /principals

  context '/principals/<name>' do
    let(:principal_name){client_name}
    let(:request_url){api_url("/principals/#{principal_name}")}

    context 'GET' do
      let(:request_method){:GET}
      include_context 'permission checks' do
        let(:admin_response){ok_response}
        let(:non_admin_response){ok_response}
        let(:validator_response){ok_response}

        # Currently, service-to-service auth does not exist in any
        # form, so all authentication is shut off for the endpoint,
        # because as far as erchef can tell, pushy (which needs this
        # endpoint) looks like a bad (unsigned) client
        let(:bad_client_response){ok_response}
      end
    end

    should_not_allow_method :POST
    should_not_allow_method :PUT
    should_not_allow_method :DELETE
  end # /principals/<name>
end
