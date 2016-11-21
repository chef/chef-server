require "spec_helper"

describe Veil::Utils do
  let(:mixed_hash) do
    {
      :foo => "bar",
      bar: "baz",
      "fizz" => :buzz
    }
  end

  let(:symbolized_hash) do
    {
      foo: "bar",
      bar: "baz",
      fizz: :buzz
    }
  end

  describe "#symbolize_keys" do
    it "symbolizes a hashes keys" do
      expect(described_class.symbolize_keys(mixed_hash)).to eq(symbolized_hash)
    end
  end
end
