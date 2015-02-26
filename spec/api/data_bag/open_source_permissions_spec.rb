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

require 'pedant/rspec/data_bag_util'
require 'pedant/opensource/permission_checks'

describe 'Data Bag Endpoint Open Source Permission Checks', :data_bags => true, :platform => :open_source do
  include Pedant::RSpec::DataBagUtil

  context "/data" do
    let(:request_url){ api_url("/data") }

    context "GET" do
      let(:request_method){:GET}
      include_context 'permission checks' do
        let(:admin_response){ok_response}
        let(:non_admin_response){ok_response}
      end
    end

    context "POST" do
      let(:request_method){:POST}
      let(:data_bag_name){"foo"}
      let(:request_payload){new_data_bag(data_bag_name)}
      let(:data_bag_url){api_url("/data/#{data_bag_name}")}
      before :each do
        delete(data_bag_url, admin_user)
      end
      after :each do
        delete(data_bag_url, admin_user)
      end
      include_context 'permission checks' do
        let(:admin_response){created_response}
        let(:non_admin_response){forbidden_response}
      end
    end

    context 'PUT' do
      let(:request_method){:PUT}
      let(:request_payload){ {"fake" => "payload"}}
      include_context 'a disallowed method'
    end

    context 'DELETE' do
      let(:request_method){:DELETE}
      include_context 'a disallowed method'
    end
  end # /data

  context '/data/<bag>' do
    let(:data_bag_name){"foo"}
    let(:request_url){api_url("/data/#{data_bag_name}")}
    before :each do
      create_data_bag(admin_user, new_data_bag(data_bag_name))
    end
    after :each do
      delete_data_bag(admin_user, data_bag_name)
    end

    context 'GET' do
      let(:request_method){:GET}
      include_context 'permission checks' do
        let(:admin_response){ok_response}
        let(:non_admin_response){ok_response}
      end
    end

    context 'POST' do
      let(:request_method){:POST}
      let(:data_bag_item_id){"foo_item"}
      let(:request_payload){new_data_bag_item(data_bag_item_id)}

      before :each do
        delete_data_bag_item(admin_user, data_bag_name, data_bag_item_id)
      end
      after :each do
        delete_data_bag_item(admin_user, data_bag_name, data_bag_item_id)
      end

      include_context 'permission checks' do
        let(:admin_response){created_response}
        let(:non_admin_response){forbidden_response}
      end
    end

    context 'PUT' do
      let(:request_method){:PUT}
      let(:request_payload){ {"fake" => "payload"}}
      include_context 'a disallowed method'
    end

    context 'DELETE' do
      let(:request_method){:DELETE}
      include_context 'permission checks' do
        let(:admin_response){ok_response}
        let(:non_admin_response){forbidden_response}
      end
    end
  end # /data/<bag>

  context '/data/<bag>/<item>' do
    let(:data_bag_name){"foo"}
    let(:data_bag){new_data_bag(data_bag_name)}
    let(:data_bag_url){api_url("/data/#{data_bag_name}")}

    let(:data_bag_item_id){"foo_item"}
    let(:data_bag_item){new_data_bag_item(data_bag_item_id)}
    let(:data_bag_item_url){api_url("/data/#{data_bag_name}/#{data_bag_item_id}")}

    let(:request_url){data_bag_item_url}

    before :each do
      create_data_bag(admin_user, data_bag)
      create_data_bag_item(admin_user, data_bag_name, data_bag_item)
    end

    after :each do
      delete_data_bag_item(admin_user, data_bag_name, data_bag_item_id)
      delete_data_bag(admin_user, data_bag_name)
    end

    context 'GET' do
      let(:request_method){:GET}
      include_context 'permission checks' do
        let(:admin_response){ok_response}
        let(:non_admin_response){ok_response}
      end
    end

    context 'POST' do
      let(:request_method){:POST}
      let(:request_payload){{"fake" => "payload"}}
      include_context 'a disallowed method'
    end

    context 'PUT' do
      let(:request_method){:PUT}
      let(:request_payload){{"id" => data_bag_item_id, "answer" => 42}}
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
  end # /data/<bag>/<item>
end
