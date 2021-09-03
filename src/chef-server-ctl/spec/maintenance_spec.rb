
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

describe "chef-server-ctl maintenance" do

  let(:command) { "maintenance" }
  let(:running_config) { { "private_chef" => { "redis_lb" => { "vip" => "127.0.0.1", "port" => "16379" } } } }
  let(:veil_creds) do
    double("ChefSecretsFile", save: true)
  end

  before do
    @helper = OmnibusCtlHelper.new(["./plugins/maintenance.rb"])
    allow(@helper.ctl).to receive(:running_config).and_return(running_config)
    allow(@helper.ctl).to receive(:credentials).and_return(veil_creds)
    allow(veil_creds).to receive(:save)
  end

  context "when an invalid argument is passed" do
    it "should return a proper error" do
      expect { @helper.run_test_omnibus_command(command, []) }
        .to raise_error(SystemExit) { |e| expect(e.status).to eq(1) }
    end
  end

end