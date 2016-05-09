require "spec_helper"
require "tempfile"

describe Veil::CredentialCollection::ChefSecretsFile do
  let(:hasher) { Veil::Hasher.create }
  let(:file) { Tempfile.new("private_chef_secrets.json") }
  let(:content) do
    {
      "veil" => {
        "type" => "Veil::CredentialCollection::ChefSecretsFile",
        "hasher" => {},
        "credentials" => {}
      }
    }
  end
  let(:legacy_content) do
    {
      "redis_lb" => {
        "password" => "f1ad3e8b1e47bc81720742a2572b9ff"
      },
      "rabbitmq" => {
        "password" => "f5031e56ae018a7b71ce153086fc88f",
        "actions_password" => "92609a68d03b50afcc597d7"
      }
    }
  end

  subject { described_class.new(hasher: hasher.to_h, path: file.path) }

  describe "#self.from_file" do
    context "when the file exists" do
      context "when it's a valid credential store" do
        it "returns an instance" do
          file.write(JSON.pretty_generate(content))
          file.rewind
          expect(described_class.from_file(file.path)).to be_instance_of(described_class)
        end
      end

      context "when it's not a valid credential store" do
        it "raises an error" do
          file.write("not a json chef secrets file")
          file.rewind
          expect { described_class.from_file(file.path) }.to raise_error(Veil::InvalidCredentialCollectionFile)
        end
      end

      context "when it's a legacy secrets file" do
        it "imports the legacy file" do
          file.write(JSON.pretty_generate(legacy_content))
          file.rewind
          instance = described_class.from_file(file.path)
          expect(instance["redis_lb"]["password"].value).to eq("f1ad3e8b1e47bc81720742a2572b9ff")
          expect(instance["redis_lb"]["password"].version).to eq(0)
          expect(instance["redis_lb"]["password"].length).to eq(31)
        end
      end
    end

    context "when the file doesn't exist" do
      it "raises an error" do
        expect { described_class.from_file("not_a_file") }.to raise_error(Veil::InvalidCredentialCollectionFile)
      end
    end
  end

  describe "#save" do
    it "saves the content to a machine loadable file" do
      file.rewind
      creds = described_class.new(path: file.path)
      creds.add("redis_lb", "password")
      creds.add("postgresql", "sql_ro_password")
      creds.save

      file.rewind
      new_creds = described_class.from_file(file.path)

      expect(new_creds["redis_lb"]["password"].value).to eq(creds["redis_lb"]["password"].value)
      expect(new_creds["postgresql"]["sql_ro_password"].value).to eq(creds["postgresql"]["sql_ro_password"].value)
    end

    it "saves the version number" do
      file.rewind
      creds = described_class.new(path: file.path, version: 12)
      creds.save

      file.rewind
      new_creds = described_class.from_file(file.path)
      expect(new_creds.version).to eq(12)
    end
  end
end
