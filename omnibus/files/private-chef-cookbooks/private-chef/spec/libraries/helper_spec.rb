require_relative '../../libraries/helper.rb'

describe OmnibusHelper, :es5 do

  before do
    # suppress log output
    allow(Chef::Log).to receive(:info)
    allow(Chef::Log).to receive(:error)
    allow(Chef::Log).to receive(:warn)
  end

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

  describe '#elastic_search_major_version' do
    let(:external) { true }
    let(:hostname) { 'myserver' }
    let(:port) { 2000 }
    let(:external_url) { "http://#{hostname}:#{port}" if hostname }
    let(:provider) {"elasticsearch"}
    let(:node) do
      {
        'private_chef' => {
          'opscode-solr4' => {
            'external' => external,
            'external_url' => external_url
          }, 
          'opscode-erchef' => {
            'search_provider' => provider
          }
        }
      }
    end

    let(:elastic_version) { '2.4.5' }
    let(:http_code) { '200' }
    let(:json_response) do <<-EOS
        {
          "name": "Zero-G",
          "cluster_name": "elasticsearch",
          "cluster_uuid": "uLwCC2LpQcqM0clO1d97AA",
          "version": {
            "number": "#{elastic_version}",
            "build_hash": "c849dd13904f53e63e88efc33b2ceeda0b6a1276",
            "build_timestamp": "2017-04-24T16:18:17Z",
            "build_snapshot": false,
            "lucene_version": "5.5.4"
          },
          "tagline": "You Know, for Search"
        }
      EOS
    end

    let(:response) do
      r = double(Net::HTTPResponse)
      allow(r).to receive(:body).and_return(json_response)
      allow(r).to receive(:code).and_return(http_code)
      r
    end

    context 'when elastic search is disabled' do
      let(:external) { false }
      let(:hostname) { nil }
      let(:port) { nil }
      let(:elastic_version) { '50.0' }

      it 'should return a default version 0' do
        helper = described_class.new(node)
        expect(helper.elastic_search_major_version).to eq(0)
      end
    end

    context 'when elastic search is unavailable' do
      it 'should return raise an exception after 5 retries.' do
        helper = described_class.new(node)
        expect(Net::HTTP).to receive(:start).with(hostname, port, use_ssl: false).exactly(5).times.and_raise('Bad connection')
        allow(helper).to receive(:sleep).and_return(0)
        expect { helper.elastic_search_major_version }.to raise_error(/Failed to connect/)
      end
    end

    context 'when elastic search is version 2' do
      it 'should return 2' do
        helper = described_class.new(node)
        expect(Net::HTTP).to receive(:start).with(hostname, port, use_ssl: false).and_return(response)
        expect(helper.elastic_search_major_version).to eq(2)
      end
    end

    context 'when elastic search is version 5' do
      let(:elastic_version) { '5.0.1' }

      it 'should return 5' do
        helper = described_class.new(node)
        expect(Net::HTTP).to receive(:start).with(hostname, port, use_ssl: false).and_return(response)
        expect(helper.elastic_search_major_version).to eq(5)
      end
    end

    context 'when elastic search is version unsupported' do
      let(:elastic_version) { '200.50.35' }

      it 'should raise an exception' do
        helper = described_class.new(node)
        expect(Net::HTTP).to receive(:start).with(hostname, port, use_ssl: false).and_return(response)
        expect { helper.elastic_search_major_version }.to raise_error(/Unsupported elasticsearch version/)
      end
    end

    context 'when elastic search returns a bad response' do
      let(:http_code) { '404' }

      it 'should raise an exception' do
        helper = described_class.new(node)
        expect(Net::HTTP).to receive(:start).with(hostname, port, use_ssl: false).and_return(response)
        expect { helper.elastic_search_major_version }.to raise_error(/Unable to interrogate/)
      end
    end

    context 'when solr is external' do
      let(:provider) {"solr"}

      it 'should return 0' do
        helper = described_class.new(node)
        expect(helper.elastic_search_major_version).to eq(0)
      end
    end

    context 'when solr is internal' do
      let(:external) { false }
      let(:hostname) { nil }
      let(:port) { nil }
      let(:provider) {"solr"}

      it 'should return a default version 0' do
        helper = described_class.new(node)
        expect(helper.elastic_search_major_version).to eq(0)
      end
    end
  end
end
