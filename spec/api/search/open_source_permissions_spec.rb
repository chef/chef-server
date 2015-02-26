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

require 'pedant/rspec/search_util'
require 'pedant/opensource/permission_checks'
require 'pedant/rspec/data_bag_util'

describe 'Search Endpoint Open Source Permission Checks', :search => true, :platform => :open_source do
  include Pedant::RSpec::SearchUtil
  include Pedant::RSpec::DataBagUtil
  include Pedant::OpenSource::PermissionChecks

  let(:admin_requestor){admin_user}

  context "/search" do
    let(:request_url){ api_url("/search") }

    context "GET" do
      let(:request_method){:GET}
      include_context 'permission checks' do
        let(:admin_response){ok_response}
        let(:non_admin_response){ok_response}
      end
    end

    should_not_allow_method :POST
    should_not_allow_method :PUT
    should_not_allow_method :DELETE
  end # /search

  def self.test_search_index(index)
    context "/search/#{index}" do
      let(:request_url){ api_url("/search/#{index}") }

      context 'GET' do
        let(:request_method){:GET}
        include_context 'permission checks' do
          let(:admin_response){ok_response}
          let(:non_admin_response){ok_response}
        end
      end

      # Partial Search
      context 'POST' do
        let(:request_method){:POST}
        # Set a minimally-acceptable payload to avoid errors
        let(:request_payload){{"foo" => ["bar"]}}
        include_context 'permission checks' do
          let(:admin_response){ok_response}
          let(:non_admin_response){ok_response}
        end
      end

      should_not_allow_method :PUT
      should_not_allow_method :DELETE
    end # /search/<index>
  end # test_search_index method

  # Test the built-in indexes
  ['node','environment','role'].each do |index|
    test_search_index(index)
  end

  context 'test the client search permissions' do
    test_search_index('client')
  end


  # test a data bag search
  context 'with a data bag' do
    test_index = 'testing_data_bag'
    include_context 'with testing data bag' do
      let(:temporary_data_bag_name){test_index}
    end
    test_search_index test_index
  end

end
