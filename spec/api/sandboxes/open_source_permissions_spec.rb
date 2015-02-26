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

require 'pedant/rspec/cookbook_util'
require 'pedant/opensource/permission_checks'

describe 'Sandbox Endpoint Open Source Permission Checks', :platform => :open_source do
  include Pedant::RSpec::CookbookUtil
  include Pedant::OpenSource::PermissionChecks

  # Until we rename the requestors
  let(:admin_requestor){admin_user}

  context '/sandboxes' do
    let(:request_url){api_url('/sandboxes')}
    should_not_allow_method :GET

    context 'POST' do
      let(:request_method){:POST}
      let(:sandbox_file){Pedant::Utility.new_random_file}
      let(:request_payload){Pedant::Sandbox.create_payload([sandbox_file])}

      include_context 'permission checks' do
        let(:admin_response){created_response}
        let(:non_admin_response){forbidden_response}
      end
    end

    should_not_allow_method :PUT
    should_not_allow_method :DELETE
  end # /sandboxes

  context '/sandboxes/<sandbox>' do
    let(:sandbox_file){Pedant::Utility.new_random_file}
    let(:sandbox){create_sandbox([sandbox_file])}
    let(:request_url){sandbox['uri']}
    let(:request_payload){{'is_completed' => true}}

    before :each do
      upload_to_sandbox(sandbox_file, sandbox)
    end

    after :each do
      # Commit the sandbox as an admin, to ensure this is always done,
      # to avoid leaving incomplete sandbox data in the database
      put(request_url, admin_requestor, :payload => request_payload)
    end

    should_not_allow_method :GET
    should_not_allow_method :POST

    context 'PUT' do
      let(:request_method){:PUT}
      include_context 'permission checks' do
        let(:admin_response){ok_response}
        let(:non_admin_response){forbidden_response}
      end
    end

    should_not_allow_method :DELETE

  end # /sandboxes/<sandbox>
end
