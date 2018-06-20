#
# Copyright 2016 Chef Software, Inc.
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

require "chef/config"
require "chef/org"
require "omnibus_ctl_helper"

describe "chef-server-ctl reindex" do
  subject(:reindex) do
    OmnibusCtlHelper.new(["./reindex.rb"]).
      run_test_omnibus_command("reindex", args)
  end
  let(:args) { [] }
  let(:config) do
    {
      'private_chef' => {}
    }
  end

  before :each do
    allow(Chef::Config).to receive(:from_file).with("/etc/opscode/pivotal.rb")
    allow(Chef::Org).to receive_message_chain(:list, :keys).and_return(
      ["testorg"]
    )
    allow(Dir).to receive(:chdir).and_yield
    allow_any_instance_of(Omnibus::Ctl).to receive(:running_config).and_return(
      config
    )
    status = double("status")
    allow(status).to receive(:success?).and_return true
    allow_any_instance_of(Omnibus::Ctl).to receive(:run_command).and_return(
      status
    )
    allow($stdout).to receive :puts
    allow($stderr).to receive :puts
  end

  context "with the -w flag" do
    context "when the search queue mode is 'batch'" do
      let(:config) do
        {
          "private_chef" => {
            "opscode-erchef" => {
              "search_queue_mode" => "batch",
            },
          },
        }
      end

      let(:args) { %w( -w --all-orgs ) }

      it "does not wait" do
        expect($stdout).to_not receive(:puts).with(
          "- Waiting for reindexing to complete"
        )
        reindex
      end

      it "logs a message" do
        expect($stderr).to receive(:puts).with(/batch/)
        reindex
      end
    end
  end
end
