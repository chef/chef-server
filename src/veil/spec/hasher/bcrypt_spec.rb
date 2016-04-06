require "spec_helper"

describe Veil::Hasher::BCrypt do
  let(:data)        { "let me enter" }
  let(:cost)        { 11 }
  let(:salt)        { "$2a$11$4xS0IHHxU5sOYZ0Z5X53Qe" }
  let(:secret)      { "only friends tell each other" }

  subject { described_class.new(secret: secret, salt: salt) }

  describe "#new" do
    it "builds an instance" do
      expect(described_class.new.class).to eq(described_class)
    end

    context "from a hash" do
      it "builds an identical instance" do
        new_instance = described_class.new(subject.to_hash)
        expect(new_instance.encrypt("slow forever")).to eq(subject.encrypt("slow forever"))
      end
    end
  end

  describe "#encrypt" do
    it "deterministically encrypts data" do
      encrypted_data = subject.encrypt(data)

      new_instance = described_class.new(
        secret: secret,
        salt: salt,
      )

      expect(new_instance.encrypt(data)).to eq(encrypted_data)
    end
  end

  describe "#to_hash" do
    it "returns itself as a hash" do
      expect(subject.to_hash).to eq({
        type: "Veil::Hasher::BCrypt",
        secret: secret,
        salt: salt,
      })
    end
  end
end
