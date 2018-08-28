#
# Copyright:: Copyright 2016-2018 Chef Software, Inc.
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
require_relative '../../libraries/preflight_bookshelf_validator.rb'
require_relative '../../libraries/private_chef.rb'
require_relative '../../libraries/helper.rb'

describe BookshelfPreflightValidator do
  context "storage_type_unchanged validation" do
    context "when no previous run exists" do
      let(:subject) {
        allow(PrivateChef).to receive(:[]).with('postgresql').and_return({})
        allow(PrivateChef).to receive(:[]).with('bookshelf').and_return({})
        s = BookshelfPreflightValidator.new({'private_chef' => {}})
        allow(s).to receive(:previous_run).and_return(nil)
        s
      }

      it "passes validation" do
        expect(subject.verify_storage_type_unchanged).to eq(true)
      end
    end

    context "when storage_type was previously unset" do
      let(:subject) {
        allow(PrivateChef).to receive(:[]).with('postgresql').and_return({})
        s = BookshelfPreflightValidator.new({'private_chef' => {}})
        allow(s).to receive(:previous_run).and_return({'bookshelf' => {}})
        allow(s).to receive(:fail_with).and_return(:i_failed)
        s
      }

      it "succeeds if set to filesystem" do
        expect(PrivateChef).to receive(:[]).with('bookshelf').and_return({'storage_type' => 'filesystem'})
        expect(subject.verify_storage_type_unchanged).to eq(true)
      end

      it "succeeds if set to :filesystem" do
        expect(PrivateChef).to receive(:[]).with('bookshelf').and_return({'storage_type' => :filesystem})
        expect(subject.verify_storage_type_unchanged).to eq(true)
      end

      it "succeeds if not set" do
        expect(PrivateChef).to receive(:[]).with('bookshelf').and_return({})
        expect(subject.verify_storage_type_unchanged).to eq(true)
      end

      it "fails if set to sql" do
        expect(PrivateChef).to receive(:[]).with('bookshelf').and_return({'storage_type' => 'sql'})
        expect(subject.verify_storage_type_unchanged).to eq(:i_failed)
      end

      it "fails if set to nonsense" do
        expect(PrivateChef).to receive(:[]).with('bookshelf').and_return({'storage_type' => 'wombatsauce'})
        expect(subject.verify_storage_type_unchanged).to eq(:i_failed)
      end
    end

    context "when storage_type was previously set" do
      let(:subject) {
        allow(PrivateChef).to receive(:[]).with('postgresql').and_return({})
        s = BookshelfPreflightValidator.new({'private_chef' => {}})
        allow(s).to receive(:previous_run).and_return({'bookshelf' => {'storage_type' => 'sql'}})
        allow(s).to receive(:fail_with).and_return(:i_failed)
        s
      }

      it "succceds if set to the same value as before" do
        expect(PrivateChef).to receive(:[]).with('bookshelf').and_return({'storage_type' => 'sql'})
        expect(subject.verify_storage_type_unchanged).to eq(true)
      end

      it "succeeds if set to the same value but as symbol" do
        expect(PrivateChef).to receive(:[]).with('bookshelf').and_return({'storage_type' => :sql})
        expect(subject.verify_storage_type_unchanged).to eq(true)
      end

      it "fails if set to a different value" do
        expect(PrivateChef).to receive(:[]).with('bookshelf').and_return({'storage_type' => 'filesystem'})
        expect(subject.verify_storage_type_unchanged).to eq(:i_failed)
      end
    end
  end
end
