require_relative '../../libraries/helper.rb'
require_relative '../../libraries/preflight_checks.rb'
require_relative '../../libraries/preflight_ipv6_validator.rb'

describe Ipv6PreflightValidator do
  let(:ipv6_preflight) do
    i = Ipv6PreflightValidator.new({'private_chef' => {}})
    allow(i).to receive(:fail_with).and_return(:i_failed)
    i
  end

  before(:each) do
    allow(File).to receive(:exists?).with("/proc/sys/net/ipv6").and_return(enabled)
    allow(IO).to receive(:read).with("/proc/sys/net/ipv6/conf/lo/disable_ipv6").and_return(disable_ipv6)
  end

  context "when ipv6 is enabled" do
    let(:enabled) { true }

    context "the loopback interface has no ipv6 address" do
      let(:disable_ipv6) { "1\n" }

      it "raises an error" do
        expect(ipv6_preflight.verify_ipv6_for_lo).to eq(:i_failed)
      end
    end

    context "the loopback interface has an ipv6 address" do
      let(:disable_ipv6) { "0\n" }

      it "does not raise an error" do
        expect(ipv6_preflight.verify_ipv6_for_lo).not_to eq(:i_failed)
      end
    end

  end

  context "when ipv6 is disabled" do
    let(:enabled) { false }
    let(:disable_ipv6) { "whatever\n" }

    it "does not raise an error" do
      expect(ipv6_preflight.verify_ipv6_for_lo).not_to eq(:i_failed)
    end
  end
end
