require "spec_helper"
require "digest"

describe Veil::Hasher::Base do
  let(:data) { "let him enter" }

  subject { described_class.new }

  describe "#hex_digest" do
    it "returns a SHA512 hex digest of the data" do
      expect(subject.send(:hex_digest, data)).to eq(OpenSSL::Digest::SHA512.hexdigest(data))
    end

    it "does not use Digest, which uses low level APIs prohibited by FIPS" do
      expect(Digest::SHA512).not_to receive(:hexdigest)
      subject.send(:hex_digest, data)
    end
  end
end
