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

describe "chef-server-ctl rotate credentials" do
  subject do
    OmnibusCtlHelper.new(["./rotate_credentials.rb"])
  end

  let(:veil_creds) do
    double("ChefSecretsFile",
           rotate: true,
           rotate_credentials: true,
           rotate_hasher: true,
           save: true)
  end

  before do
    allow(subject.ctl).to receive(:ensure_configured!).and_return(true)
    allow(subject.ctl).to receive(:backup_secrets_file).and_return("/tmp/backup.json")
    allow(subject.ctl).to receive(:restore_secrets_file).and_return(true)
    allow(subject.ctl).to receive(:remove_backup_file).and_return(true)
    allow(subject.ctl).to receive(:run_chef).and_return(double("ProcessStatus", success?: true))
    allow(Veil::CredentialCollection::ChefSecretsFile).to receive(:from_file).and_return(veil_creds)
  end

  shared_examples "rotation command" do
    it "exits with 1 when the chef run fails" do
      allow(subject.ctl).to receive(:run_chef).and_return(double("ProcessStatus", success?: false))
      expect { subject.run_test_omnibus_command(command, params) }
        .to raise_error(SystemExit) { |e| expect(e.status).to eq(1) }
    end

    it "exits with 0 when the chef run succeeds" do
      expect { subject.run_test_omnibus_command(command, params) }
        .to raise_error(SystemExit) { |e| expect(e.status).to eq(0) }
    end

    it "backs up the secrets file" do
      expect(subject.ctl).to receive(:backup_secrets_file)
      expect { subject.run_test_omnibus_command(command, params) }
        .to raise_error(SystemExit) { |e| expect(e.status).to eq(0) }
    end

    it "restores the old file if rotation fails" do
      %i{rotate rotate_credentials rotate_hasher}.each do |rotation_command|
        allow(veil_creds).to receive(rotation_command).and_raise
      end

      expect(subject.ctl).to receive(:restore_secrets_file)
      expect { subject.run_test_omnibus_command(command, params) }
        .to raise_error(SystemExit) { |e| expect(e.status).to eq(1) }
    end
  end

  context "rotate-credentials" do
    let(:command) { "rotate-credentials" }
    let(:params) { ["opscode-solr4"] }

    it_behaves_like "rotation command"
  end

  context "rotate-all-credentials" do
    let(:command) { "rotate-all-credentials" }
    let(:params) { [] }

    it_behaves_like "rotation command"
  end

  context "rotate-shared-secrets" do
    let(:command) { "rotate-shared-secrets" }
    let(:params) { [] }

    it_behaves_like "rotation command"
  end

  context "require-credential-rotation" do
    let(:ui) { HighLine.new }
    let(:prehook_file_path) { "/tmp/prehook" }
    let(:prehook_file) { StringIO.new }

    before do
      allow(HighLine).to receive(:new).and_return(ui)
      allow(subject.ctl).to receive(:run_sv_command).with("stop").and_return(true)
      allow(subject.ctl).to receive(:get_all_services).and_return([])
      allow(subject.ctl).to receive(:run_sv_command).with("stop")
      allow(subject.ctl)
        .to receive(:credential_prehook_file_path)
        .and_return(prehook_file_path)
      allow(File).to receive(:open).with(prehook_file_path, "a+").and_yield(prehook_file)
    end

    it "bypasses cofirmation if passed --yes" do
      expect(ui).to_not receive(:ask)

      expect { subject.run_test_omnibus_command("require-credential-rotation", %w{--yes}) }
        .to raise_error(SystemExit) { |e| expect(e.status).to eq(0) }
    end

    it "prompts the user for confirmation" do
      allow(ui).to receive(:ask).and_return("yes")
      expect(ui).to receive(:ask).with(/confirm/)

      expect { subject.run_test_omnibus_command("require-credential-rotation", []) }
        .to raise_error(SystemExit) { |e| expect(e.status).to eq(0) }
    end

    it "exits if the user does not agree" do
      allow(ui).to receive(:ask).and_return("no")
      expect(ui).to receive(:ask).with(/confirm/)

      expect { subject.run_test_omnibus_command("require-credential-rotation", []) }
        .to raise_error(SystemExit) { |e| expect(e.status).to eq(1) }
    end

    it "stops all services" do
      expect(subject.ctl).to receive(:run_sv_command).with("stop")
      expect { subject.run_test_omnibus_command("require-credential-rotation", %w{--yes}) }
        .to raise_error(SystemExit) { |e| expect(e.status).to eq(0) }
    end

    it "disables all services" do
      allow(subject.ctl)
        .to receive(:get_all_services)
        .and_return(%w{postgresql oc_erchef})
      allow(subject.ctl).to receive(:service_enabled?).and_return(true)
      allow(File).to receive(:unlink).with(/postgresq/).and_return(true)
      allow(File).to receive(:unlink).with(/oc_erchef/).and_return(true)
      expect { subject.run_test_omnibus_command("require-credential-rotation", %w{--yes}) }
        .to raise_error(SystemExit) { |e| expect(e.status).to eq(0) }
    end

    it "writes the pre-hook lock" do
      expect(prehook_file).to receive(:write).with(/require_credential_rotation/)
      expect { subject.run_test_omnibus_command("require-credential-rotation", %w{--yes}) }
        .to raise_error(SystemExit) { |e| expect(e.status).to eq(0) }
    end
  end
end
