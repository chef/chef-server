require "spec_helper"

describe Veil::Hasher::Base do
  let(:data) { "let him enter" }

  subject { described_class.new }

  describe "#hex_digest" do
    it "returns a SHA512 hex digest of the data" do
      expect(subject.send(:hex_digest, data)).to eq(Digest::SHA512.hexdigest(data))
    end
  end
end
