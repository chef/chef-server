#
# Copyright:: 2020 Chef Software, Inc.
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
#
require_relative '../../libraries/preflight_indexing_validator.rb'
require_relative '../../libraries/private_chef.rb'
require_relative '../../libraries/helper.rb'

describe IndexingPreflightValidator do
  let(:indexing_preflight) do
    s = IndexingPreflightValidator.new('private_chef' => {
      'opscode-erchef' => {
        'reindex_sleep_min_ms' => 500,
        'reindex_sleep_max_ms' => 2000,
      },
      'postgresql' => {} })
    allow(s).to receive(:fail_with).and_return(:i_failed)
    s
  end

  before(:each) do
    allow(PrivateChef).to receive(:[]).with('elasticsearch').and_return({})
    allow(PrivateChef).to receive(:[]).with('opscode_solr4').and_return({})
    allow(PrivateChef).to receive(:[]).with('postgresql').and_return({})
  end

  context 'when min and max sleep time has been given in the config' do
    before(:each) do
      allow(PrivateChef).to receive(:[]).with('opscode_erchef').and_return(
        'reindex_sleep_min_ms' => min_sleep_time,
        'reindex_sleep_max_ms' => max_sleep_time
      )
    end

    context 'when min is greater then max' do
      let(:min_sleep_time) { 100 }
      let(:max_sleep_time) { 2 }

      it 'raises an error' do
        expect(indexing_preflight.verify_consistent_reindex_sleep_times).to eq(:i_failed)
      end
    end
  end

  context 'when only min sleep time has been given in the config' do
    before(:each) do
      allow(PrivateChef).to receive(:[]).with('opscode_erchef').and_return(
        'reindex_sleep_min_ms' => min_sleep_time
      )
    end

    context 'when min is greater then max' do
      let(:min_sleep_time) { 2001 }

      it 'raises an error' do
        expect(indexing_preflight.verify_consistent_reindex_sleep_times).to eq(:i_failed)
      end
    end
  end

  context 'when only max sleep time has been given in the config' do
    before(:each) do
      allow(PrivateChef).to receive(:[]).with('opscode_erchef').and_return(
        'reindex_sleep_max_ms' => max_sleep_time
      )
    end

    context 'when min is greater then max' do
      let(:max_sleep_time) { 499 }

      it 'raises an error' do
        expect(indexing_preflight.verify_consistent_reindex_sleep_times).to eq(:i_failed)
      end
    end
  end
end
