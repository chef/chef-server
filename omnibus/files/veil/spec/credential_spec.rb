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
      length: 66,
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
end
