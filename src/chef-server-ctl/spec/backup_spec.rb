#
# Copyright 2015 Chef Software, Inc.
#
# Licensed under the Apache License, Version 2.0 (the 'License');
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an 'AS IS' BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

require "omnibus_ctl_helper"

describe "chef-server-ctl backup" do
  before do
    @omnibus_ctl = OmnibusCtlHelper.new(["./plugins/backup.rb"])
    allow(@omnibus_ctl.ctl).to receive(:running_config).and_return(running_config)
    allow(ChefBackup::Runner).to receive(:new).and_return(runner)
  end

  let(:runner) { double("ChefBackup::Runner") }
  let(:running_config) { { "private_chef" => { "backup" => { "strategy" => "tar" } } } }

  context "when the backup fails" do
    before do
      allow(runner)
        .to receive(:backup)
        .and_raise(ArgumentError, "Something went terribly wrong")
    end

    it "exits with 1" do
      expect { @omnibus_ctl.run_test_omnibus_command("backup", []) }
        .to raise_error(SystemExit) { |e| expect(e.status).to eq(1) }
    end
  end

  context "when the backup succeeds" do
    before { allow(runner).to receive(:backup).and_return(nil) }

    it "exits with 0" do
      expect { @omnibus_ctl.run_test_omnibus_command("backup", []) }
        .to raise_error(SystemExit) { |e| expect(e.status).to eq(0) }
    end
  end
end
