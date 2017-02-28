#
# Copyright:: Copyright (c) 2016 Chef Software, Inc.
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
require_relative '../../libraries/preflight_bootstrap_validator.rb'
require_relative '../../libraries/private_chef.rb'
require_relative '../../libraries/helper.rb'


describe BootstrapPreflightValidator do
  let(:subject) { BootstrapPreflightValidator.new(node_object) }

  describe "#bypass_bootstrap?" do
    context "when a previous run exists" do
      let(:node_object) do { "private_chef" => {},
                             "previous_run" => {"some" => "has"}}
      end

      # We end up skipping the bootstrap in most cases here because if
      # a previous run exists we likely have a bootstrap sentinal file
      # so OmnibusHelper.has_been_bootstrapped? will be true and that
      # function is used in conjunction with this when determinging
      # whether to run the bootstrap recipe.
      it "counter-intuitively returns false" do
        expect(subject.bypass_bootstrap?).to eq(false)
      end
    end

    context "when no previous run exists" do
      let(:node_object) do
        { "private_chef" => {} }
      end

      context "when the secrets file doesn't exist" do
        before do
          credentials = double(Object)
          allow(PrivateChef).to_receive(:credentials?).and_return(credentials)
          allow(credentials).to receive(:exist?).with('chef-server', 'superuser_key').and_return(true)
          allow(File).to receive(:exist?).with("/etc/opscode/private-chef-secrets.json").and_return(false)
        end

        it "returns false" do
          expect(subject.bypass_bootstrap?).to eq(false)
        end
      end

      context "when the pivotal.pem file doesn't exist" do
        before do
          allow(File).to receive(:exist?).with("/etc/opscode/pivotal.pem").and_return(false)
          allow(File).to receive(:exist?).with("/etc/opscode/private-chef-secrets.json").and_return(true)
        end

        it "returns false" do
          expect(subject.bypass_bootstrap?).to eq(false)
        end
      end

      context "when both private-chef-secrets.json and pivotal.pem exist" do
        before do
          allow(File).to receive(:exist?).with("/etc/opscode/pivotal.pem").and_return(true)
          allow(File).to receive(:exist?).with("/etc/opscode/private-chef-secrets.json").and_return(true)
          allow(Veil::CredentialCollection::ChefSecretsFile).to receive(:from_file)
                                                                 .with("/etc/opscode/private-chef-secrets.json")
                                                                 .and_return(double("SecretsFile", size: 10))
        end


        context "when postgresql is running externally" do
          before do
            allow(PrivateChef).to receive(:[]).with('postgresql').and_return({"external" => true})
          end

          it "returns true" do
            expect(subject.bypass_bootstrap?).to eq(true)
          end
        end

        context "when postgresql is running locally" do
          before do
            allow(PrivateChef).to receive(:[]).with('postgresql').and_return({"external" => false})
          end

          it "returns false" do
            expect(subject.bypass_bootstrap?).to eq(false)
          end
        end
      end
    end
  end
end
