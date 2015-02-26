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

RSpec.shared_examples "Cookbook API Open Source Permissions" do
  include Pedant::RSpec::CookbookUtil
  include Pedant::OpenSource::PermissionChecks

  # Until we rename the requestors
  let(:admin_requestor){admin_user}

  context '/cookbooks' do
    let(:request_url){api_url("/#{cookbook_url_base}")}

    context 'GET' do
      let(:request_method){:GET}
      include_context 'permission checks' do
        let(:admin_response){ok_response}
        let(:non_admin_response){ok_response}
      end
    end

    should_not_allow_method :POST
    should_not_allow_method :PUT
    should_not_allow_method :DELETE
  end

  context '/cookbooks/_latest' do
    let(:request_url){api_url("/#{cookbook_url_base}/_latest")}

    context 'GET' do
      let(:request_method){:GET}
      include_context 'permission checks' do
        let(:admin_response){ok_response}
        let(:non_admin_response){ok_response}
      end
    end

    should_not_allow_method :POST
    should_not_allow_method :PUT
    should_not_allow_method :DELETE
  end

  context '/cookbooks/_recipes' do
    let(:request_url){api_url("/#{cookbook_url_base}/_recipes")}

    context 'GET' do
      let(:request_method){:GET}
      include_context 'permission checks' do
        let(:admin_response){ok_response}
        let(:non_admin_response){ok_response}
      end
    end

    should_not_allow_method :POST
    should_not_allow_method :PUT
    should_not_allow_method :DELETE
  end

  context '/cookbooks/<cookbook>' do
    include_context 'with temporary testing cookbook'
    let(:request_url){api_url("/#{cookbook_url_base}/#{cookbook_name}")}

    context 'GET' do
      let(:request_method){:GET}
      include_context 'permission checks' do
        let(:admin_response){ok_response}
        let(:non_admin_response){ok_response}
      end
    end

    should_not_allow_method :POST
    should_not_allow_method :PUT
    should_not_allow_method :DELETE
  end


  context '/cookbooks/<cookbook>/<version>' do
    let(:request_url){api_url("/#{cookbook_url_base}/#{cookbook_name}/#{cookbook_version}")}
    include_context 'with temporary testing cookbook'

    context 'GET' do
      let(:request_method){:GET}
      include_context 'permission checks' do
        let(:admin_response){ok_response}
        let(:non_admin_response){ok_response}
      end
    end

    should_not_allow_method :POST

    # Cookbook creation
    context 'PUT' do
      let(:request_method){:PUT}
      # Since cookbook creation is via a PUT directly to the cookbook
      # resource, rather than a POST to a "container" resource (like
      # everything else), we aren't using the temporary cookbook
      # introduced by the 'with temporary testing cookbook' shared
      # context for these tests.
      #
      # Instead, we are generating our own cookbook for these PUT
      # tests, and cleaning up after ourselves.
      let(:cookbook_name){"foo"}
      let(:cookbook_version){"6.6.6"}
      let(:request_payload){new_cookbook(cookbook_name, cookbook_version)}

      after(:each) do
        delete_cookbook(admin_user, cookbook_name, cookbook_version)
      end

      include_context 'permission checks' do
        let(:admin_response){created_response}
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
  end

end
