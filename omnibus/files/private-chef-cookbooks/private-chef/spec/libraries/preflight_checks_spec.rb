#
# Copyright:: 2017-2018 Chef Software, Inc.
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

require_relative '../../libraries/private_chef.rb'
require_relative '../../libraries/helper.rb'

describe PreflightChecks do
  let(:credentials) { double('Credentials') }
  let(:node_object) do
    {
      'private_chef' => {},
      'previous_run' => { 'some' => 'has' },
    }
  end

  let(:subject) { PreflightValidator.new(node_object) }

  describe '#first_run?' do
    context 'when no previous run exists' do
      let(:node_object) { { 'private_chef' => {} } }
      it 'replies true' do
        expect(subject.first_run?).to be true
      end
    end

    context 'when a previous run does exist' do
      let(:node_object) { { 'private_chef' => {}, 'previous_run' => { 'a' => 'b' } } }
      # default node_object has
      it 'replies false' do
        expect(subject.first_run?).to be false
      end
    end
  end

  describe '#secrets_exists?' do
    # If true, Veil will tell us that the superuser/webui keys exist in the store.
    let(:keys_exist) { true }
    # Adjust this for number of secrets you want Veil to say it has
    let(:num_secrets) { 1 }
    let(:secret_count) do
      num_secrets + (keys_exist ? 2 : 0)
    end
    let(:credentials) { double('Credentials') }

    before do
      allow(credentials).to receive(:exist?).with("#{Chef::Dist::Server::SHORT}", anything).and_return keys_exist
      allow(credentials).to receive(:length).and_return secret_count
      allow(PrivateChef).to receive(:credentials).and_return credentials
    end

    context 'when no application keys have been stored' do
      let(:keys_exist) { false }
      context 'and there are no other secrets loaded' do
        let(:num_secrets) { 0 }
        it 'returns false' do
          expect(subject.secrets_exists?).to be false
        end
      end
      context 'and there are other secrets loaded' do
        let(:num_secrets) { 1 }
        it 'returns true' do
          expect(subject.secrets_exists?).to be true
        end
      end
    end

    context 'when application keys have been stored' do
      let(:keys_exist) { true }
      context 'and there are no other secrets loaded' do
        let(:num_secrets) { 0 }
        it 'returns false' do
          expect(subject.secrets_exists?).to be false
        end
      end
      context 'and there are other secrets loaded' do
        let(:num_secrets) { 1 }
        it 'returns true' do
          expect(subject.secrets_exists?).to be true
        end
      end
    end
  end
end
