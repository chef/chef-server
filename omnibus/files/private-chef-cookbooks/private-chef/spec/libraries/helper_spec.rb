require_relative '../../libraries/helper.rb'

describe OmnibusHelper do
  describe '#bookshelf_s3_url' do
    let(:node) do
      {
        'private_chef' => {
          'nginx' => {
            'x_forwarded_proto' => 'https'
          },
          'bookshelf' => {
            'vip_port' => 8443
          }
        }
      }
    end

    it 'returns a properly formatted URL' do
      helper = described_class.new(node)
      allow(helper).to receive(:vip_for_uri).with('bookshelf').and_return('bookshelf-vip')
      expect(helper.bookshelf_s3_url).to eq('https://bookshelf-vip:8443')
    end
  end

  describe "escape_characters_in_string" do
    subject(:helper) { described_class.escape_characters_in_string(input_string) }
    let(:input_string) { "foo'" }

    it "escapes special characters" do
      expect(helper).to eq("foo\\'")
    end

    context "with nil" do
      let(:input_string) { nil }

      it "returns an empty string" do
        expect(helper).to eq("")
      end
    end
  end

  describe '#nginx_ssl_url' do
    let(:node) do
      {
        'private_chef' => {
          'nginx' => {
            'url' => 'https://nginx-url',
            'ssl_port' => 8443
          }
        }
      }
    end

    it 'returns a properly formatted URL' do
      helper = described_class.new(node)
      expect(helper.nginx_ssl_url).to eq('https://nginx-url:8443')
    end
  end
end
