require "spec_helper"

describe Veil::Hasher::PBKDF2 do
  let(:group)       { "android" }
  let(:name)        { "artoo" }
  let(:version)     { 2 }
  let(:salt)        { "nacl" }
  let(:secret)      { "sauce" }
  let(:iterations)  { 100 }
  let(:digest)      { "SHA256" }

  subject do
    described_class.new(
      secret: secret,
      salt: salt,
      iterations: iterations,
      hash_function: digest
    )
  end

  describe "#new" do
    it "builds an instance" do
      expect(described_class.new.class).to eq(described_class)
    end

    context "from a hash" do
      it "builds an identical instance" do
        new_instance = described_class.new(subject.to_hash)
        expect(new_instance.encrypt("slow", "forever", 5)).to eq(subject.encrypt("slow", "forever", 5))
      end
    end
  end

  describe "#encrypt" do
    it "deterministically encrypts data" do
      encrypted_data = subject.encrypt(group, name, version)

      new_instance = described_class.new(
        secret: secret,
        salt: salt,
        iterations: iterations,
        hash_function: digest
      )

      expect(new_instance.encrypt(group, name, version)).to eq(encrypted_data)
    end
  end

  describe "#to_hash" do
    it "returns itself as a hash" do
      expect(subject.to_hash).to eq({
        type: "Veil::Hasher::PBKDF2",
        secret: secret,
        salt: salt,
        iterations: iterations,
        hash_function: "OpenSSL::Digest::SHA256"
      })
    end
  end
end
