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

require 'pedant/rspec/knife_util'
require 'pedant/rspec/cookbook_util'

describe 'knife', :knife do
  context 'cookbook' do
    context 'download' do
      include Pedant::RSpec::KnifeUtil
      include Pedant::RSpec::CookbookUtil
      let(:cookbook_url_base) { "cookbooks" }
      let(:command) { "knife cookbook download #{cookbook_name} -c #{knife_config}" }
      let(:cwd) { downloaded_cookbook_dir }
      let(:downloaded_cookbook_dir) { knife_fixture "cookbooks" }

      before(:each) { upload_cookbook(admin_user, cookbook_name, version, cookbook_payload) }
      after(:each)  { delete_cookbook admin_user, cookbook_name, version }

      let(:cookbook_name) { Pedant::Utility.with_unique_suffix("pedant-cookbook") }
      let(:version) { "0.0.1" }

      let(:cookbook_payload) do
        new_cookbook(cookbook_name, version).tap do |c|
          c['metadata']['providing'] = metadata_providing
        end
      end
      let(:metadata_providing) { { "farthing" => "0.0.1" } }

      context 'as an admin' do
        let(:requestor) { knife_admin }

        it 'should succeed' do
          should have_outcome :status => 0, :stderr => /Cookbook downloaded to.*#{cookbook_name}/
        end
      end

      context 'as a normal user' do
        let(:requestor) { knife_user }
        it 'should succeed' do
          should have_outcome :status => 0, :stderr => /Cookbook downloaded to.*#{cookbook_name}/
        end
      end
    end
  end
end
