require "spec_helper"

describe Veil::Hasher do
  let(:data)        { "let him enter" }
  let(:cost)        { 11 }
  let(:salt)        { "$2a$11$4xS0IHHxU5sOYZ0Z5X53Qe" }
  let(:secret)      { "super duper secret" }
  let(:hash) do
    { type: "Veil::Hasher::BCrypt",
      secret: secret,
      salt: salt
    }
  end

  describe "#self.create" do
    context "with opts" do
      it "returns an instance of the class" do
        instance = described_class.create(hash)
        expect(instance.class.name).to eq("Veil::Hasher::BCrypt")
        expect(instance.secret).to eq(secret)
        expect(instance.salt).to eq(salt)
      end
    end

    context "without opts" do
      it "returns an instance of the default class" do
        instance = described_class.create
        expect(instance.class.name).to eq("Veil::Hasher::PBKDF2")
        expect(instance.iterations).to eq(10_000)
        expect(instance.hash_function.class.name).to eq("OpenSSL::Digest::SHA512")
      end
    end
  end
end
