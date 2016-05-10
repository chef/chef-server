require "spec_helper"

describe Veil::Credential do
  let(:data)        { "heart and lungs" }
  let(:salt)        { "NaCL" }
  let(:secret)      { "Black Eagle" }
  let(:hasher_hash) do
    {
      data: data,
      salt: salt,
      secret: secret
    }
  end

  let(:credential_hash) do
    {
      name: "eros",
      version: 23,
      value: "some crazy secret",
      length: 17,
      group: "portals"
    }
  end

  let(:hasher) { Veil::Hasher.create(hasher_hash) }

  subject { described_class.new(name: "eros", value: "thing") }

  describe "self.create" do
    it "creates an instance from the hash" do
      cred = described_class.create(credential_hash)
      expect(cred.name).to eq(credential_hash[:name])
      expect(cred.version).to eq(credential_hash[:version])
      expect(cred.value).to eq(credential_hash[:value])
      expect(cred.length).to eq(credential_hash[:length])
      expect(cred.group).to eq(credential_hash[:group])
    end
  end

  describe "#new" do
    it "sets a default version to 0" do
      expect(subject.version).to eq(0)
    end
  end

  describe "#rotate!" do
    it "increments the version and hashes a new value" do
      cred = described_class.new(group: "foo", name: "bar", version: 2, value: "thing")
      cred.rotate!(hasher)
      expect(cred.version).to eq(3)
      expect(cred.value).to_not eq("thing")
    end

    context "when the hasher is invalid" do
      it "raises an invalid hasher error" do
        expect { subject.rotate!(1) }.to raise_error(Veil::InvalidHasher)
      end
    end

    context "when the credential is frozen" do
      it "raises a runtime error" do
        cred = described_class.new(group: "foo", name: "bar", version: 3, value: "thing", frozen: true)
        expect { cred.rotate!(1) }.to raise_error(RuntimeError)
      end
    end
  end

  describe "#rotate" do
    it "increments the version and hashes a new value" do
      cred = described_class.new(group: "foo", name: "bar", version: 2, value: "thing")
      cred.rotate(hasher)
      expect(cred.version).to eq(3)
      expect(cred.value).to_not eq("thing")
    end

    context "when the hasher is invalid" do
      it "does not rotate the credential" do
        cred = described_class.new(group: "foo", name: "bar", version: 3, value: "thing", frozen: true)
        expect(cred.rotate(1)).to eq(false)
        expect(cred.version).to eq(3)
        expect(cred.value).to eq("thing")
      end
    end

    context "when the credential is frozen" do
      it "does not rotate the credential" do
        cred = described_class.new(group: "foo", name: "bar", version: 3, value: "thing", frozen: true)
        expect(cred.rotate(hasher)).to eq(false)
        expect(cred.version).to eq(3)
        expect(cred.value).to eq("thing")
      end
    end
  end
end
