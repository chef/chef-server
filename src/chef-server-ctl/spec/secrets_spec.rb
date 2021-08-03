#
# Copyright 2017 Chef Software, Inc.
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
    OmnibusCtlHelper.new(["./plugins/secrets.rb"])
  end

  let(:veil_creds) { double("ChefSecretsFile", save: true) }

  before do
    allow(subject.ctl).to receive(:credentials).and_return(veil_creds)
    allow(veil_creds).to receive(:save)
  end

  context "when an enabled service depends on the changed secret" do
    before do
      allow(veil_creds).to receive(:add).with("bookshelf", "access_key_id", { value: "new_key", frozen: true, force: true })
      allow(subject.ctl).to receive(:service_enabled?).with("bookshelf").and_return(true)
      allow(subject.ctl).to receive(:service_enabled?).with("opscode-erchef").and_return(true)
    end

    it "prompts user to restart services" do
      expect(subject.ctl).to_not receive(:run_sv_command_for_service)
      expect {
        subject.run_test_omnibus_command("set-secret", %w{bookshelf access_key_id new_key})
      }.to output("Please restart these services: bookshelf, opscode-erchef\n").to_stdout
    end

    it "restarts services with --with-restart flag" do
      expect(subject.ctl).to receive(:run_sv_command_for_service).with("restart", "bookshelf")
      expect(subject.ctl).to receive(:run_sv_command_for_service).with("restart", "opscode-erchef")
      expect {
        subject.run_test_omnibus_command("set-secret", ["bookshelf", "access_key_id", "new_key", "--with-restart"])
      }.to output("Restarting these services: bookshelf, opscode-erchef\n").to_stdout
    end
  end

  # This is an unlikely scenario, but worth guarding against confusing messaging about restarting services that are not installed/enabled.
  context "when an enabled service and a not-enabled service depend on the changed secret" do
    before do
      allow(veil_creds).to receive(:add).with("bookshelf", "access_key_id", { value: "new_key", frozen: true, force: true })
      allow(subject.ctl).to receive(:service_enabled?).with("bookshelf").and_return(true)
      allow(subject.ctl).to receive(:service_enabled?).with("opscode-erchef").and_return(false)
    end

    it "prompts user to restart only enabled services" do
      expect(subject.ctl).to_not receive(:run_sv_command_for_service)
      expect {
        subject.run_test_omnibus_command("set-secret", %w{bookshelf access_key_id new_key})
      }.to output("Please restart these services: bookshelf\n").to_stdout
    end

    it "restarts only enabled services with --with-restart flag" do
      expect(subject.ctl).to receive(:run_sv_command_for_service).with("restart", "bookshelf")
      expect(subject.ctl).to_not receive(:run_sv_command_for_service).with("restart", "opscode-erchef")
      expect {
        subject.run_test_omnibus_command("set-secret", ["bookshelf", "access_key_id", "new_key", "--with-restart"])
      }.to output("Restarting these services: bookshelf\n").to_stdout
    end
  end

  # Also unlikely that we'd change a secret for a not-enabled service.
  context "when only non-enabled services depend on the changed secret" do
    before do
      allow(veil_creds).to receive(:add).with("manage", "secret_token", { value: "new_key", frozen: true, force: true })
      allow(subject.ctl).to receive(:service_enabled?).with("chef-manage").and_return(false)
    end

    it "does not prompt user to restart services" do
      expect(subject.ctl).to_not receive(:run_sv_command_for_service)
      expect {
        subject.run_test_omnibus_command("set-secret", %w{manage secret_token new_key})
      }.to_not output.to_stdout
    end

    it "does not restart services with --with-restart flag" do
      expect(subject.ctl).to_not receive(:run_sv_command_for_service)
      expect {
        subject.run_test_omnibus_command("set-secret", ["manage", "secret_token", "new_key", "--with-restart"])
      }.to_not output.to_stdout
    end
  end

  # chef-manage follows a different path as it has several of its own services.
  context "when chef-manage depends on the changed secret and is installed" do
    before do
      allow(veil_creds).to receive(:add).with("manage", "secret_key_base", { value: "new_key", frozen: true, force: true })
      allow(File).to receive(:exist?).with("/opt/chef-manage/sv/").and_return(true)
    end

    it "prompts user to restart chef-manage" do
      expect {
        subject.run_test_omnibus_command("set-secret", %w{manage secret_key_base new_key})
      }.to output("Please restart these services: chef-manage\n").to_stdout
    end

    it "restarts chef-manage with --with-restart flag" do
      expect_any_instance_of(Mixlib::ShellOut).to receive(:run_command)
      expect {
        subject.run_test_omnibus_command("set-secret", ["manage", "secret_key_base", "new_key", "--with-restart"])
      }.to output("Restarting these services: chef-manage\n").to_stdout
    end
  end

  context "when chef-manage depends on the changed secret but is not installed" do
    before do
      allow(veil_creds).to receive(:add).with("manage", "secret_key_base", { value: "new_key", frozen: true, force: true })
      allow(File).to receive(:exist?).with("/opt/chef-manage/sv/").and_return(false)
    end

    it "does not prompt user to restart chef-manage" do
      expect {
        subject.run_test_omnibus_command("set-secret", %w{manage secret_key_base new_key})
      }.to_not output.to_stdout
    end

    it "does not restart chef-manage with --with-restart flag" do
      expect_any_instance_of(Mixlib::ShellOut).to_not receive(:run_command)
      expect {
        subject.run_test_omnibus_command("set-secret", ["manage", "secret_key_base", "new_key", "--with-restart"])
      }.to_not output.to_stdout
    end
  end
end
