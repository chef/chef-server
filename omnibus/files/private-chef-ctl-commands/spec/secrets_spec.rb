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

require "omnibus_ctl_helper"

module Omnibus
  class Ctl
    def credentials
      # This is defined in chef-server-ctl, but chef-server-ctl isn't
      # actually loaded as part of these tests and attempting to load
      # it would require some code changes since it expects to be able
      # to run the command specified in ARGV.
    end
  end
end

describe "chef-server-ctl set-secret" do
  subject do
    OmnibusCtlHelper.new(["./secrets.rb"])
  end

  let(:veil_creds) { double("ChefSecretsFile", save: true) }

  before do
    allow(subject.ctl).to receive(:credentials).and_return(veil_creds)
    allow(veil_creds).to receive(:add).with("bookshelf", "access_key_id", {:value=>"new_key", :frozen=>true, :force=>true})
    allow(veil_creds).to receive(:save)
  end

  context "set-secret" do
    it "prompts user to restart required services" do
      expect {
        subject.run_test_omnibus_command("set-secret", ['bookshelf', 'access_key_id', 'new_key'])
      }.to output("You have changed access_key_id for bookshelf. Please restart these necessary services: bookshelf, oc_erchef\n").to_stdout
    end

    it "restarts necessary services with --with-restart flag" do
      expect(subject.ctl).to receive(:run_sv_command).with("restart", "bookshelf")
      expect(subject.ctl).to receive(:run_sv_command).with("restart", "oc_erchef")
      subject.run_test_omnibus_command("set-secret", ['bookshelf', 'access_key_id', 'new_key', '--with-restart'])
    end
  end
end
