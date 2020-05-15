require_relative '../../libraries/helper.rb'

describe OmnibusHelper do
  before do
    # suppress log output
    allow(Chef::Log).to receive(:info)
    allow(Chef::Log).to receive(:error)
    allow(Chef::Log).to receive(:warn)
  end

  describe '#is_ip?' do
    it 'returns true for an IPv4 address' do
      expect(OmnibusHelper.is_ip?('192.168.33.10')).to eq(true)
    end

    it 'returns true for an IPv6 address' do
      expect(OmnibusHelper.is_ip?('::1')).to eq(true)
    end

    it 'returns false for a hostname' do
      expect(OmnibusHelper.is_ip?('1.example.com')).to eq(false)
    end
  end

  describe '#bookshelf_s3_url' do
    let(:node) do
      {
        'private_chef' => {
          'nginx' => {
            'x_forwarded_proto' => 'https',
          },
          'bookshelf' => {
            'vip_port' => 8443,
          },
        },
      }
    end

    it 'returns a properly formatted URL' do
      helper = described_class.new(node)
      allow(helper).to receive(:vip_for_uri).with('bookshelf').and_return('bookshelf-vip')
      expect(helper.bookshelf_s3_url).to eq('https://bookshelf-vip:8443')
    end
  end

  describe 'escape_characters_in_string' do
    subject(:helper) { described_class.escape_characters_in_string(input_string) }
    let(:input_string) { "foo'" }

    it 'escapes special characters' do
      expect(helper).to eq("foo\\'")
    end

    context 'with nil' do
      let(:input_string) { nil }

      it 'returns an empty string' do
        expect(helper).to eq('')
      end
    end
  end

  describe '#nginx_ssl_url' do
    let(:node) do
      {
        'private_chef' => {
          'nginx' => {
            'url' => 'https://nginx-url',
            'ssl_port' => 8443,
          },
        },
      }
    end

    it 'returns a properly formatted URL' do
      helper = described_class.new(node)
      expect(helper.nginx_ssl_url).to eq('https://nginx-url:8443')
    end
  end

  describe '#elastic_search_major_version' do
    let(:external) { true }
    let(:external_url) { 'http://myserver:2000' }
    let(:provider) { 'elasticsearch' }
    let(:node) do
      {
        'private_chef' => {
          'opscode-solr4' => {
            'external' => external,
            'external_url' => external_url,
          },
          'opscode-erchef' => {
            'search_provider' => provider,
          },
        },
      }
    end

    let(:elastic_version) { '2.4.5' }
    let(:response) do
      <<-EOS
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
    let(:client) { double(Chef::HTTP) }

    context 'when elastic search is disabled' do
      let(:provider) { 'solr' }
      let(:elastic_version) { '50.0' }

      it 'should return a default version 0' do
        helper = described_class.new(node)
        expect(helper.elastic_search_major_version).to eq(0)
      end
    end

    shared_context 'elastic search version request fails' do
      it 'should return 2 but some of the requests will fail' do
        helper = described_class.new(node)
        allow(Chef::HTTP).to receive(:new).with(external_url).and_return(client)

        expect(client).to receive(:get).with('').exactly(times).times.and_raise('Bad connection')
        expect(client).to receive(:get).with('').and_return(response)

        allow(helper).to receive(:sleep).and_return(0)
        expect(helper.elastic_search_major_version).to eq(2)
      end
    end

    context 'when elastic search is unavailable' do
      context 'should return 2 but the request fails first time only' do
        let(:times) { 1 }
        it_behaves_like 'elastic search version request fails'
      end

      context 'should return 2 but the request fails the first two times' do
        let(:times) { 2 }
        it_behaves_like 'elastic search version request fails'
      end

      context 'should return 2 but the request fails the first three times' do
        let(:times) { 3 }
        it_behaves_like 'elastic search version request fails'
      end

      context 'should return 2 but the request fails the first four times' do
        let(:times) { 4 }
        it_behaves_like 'elastic search version request fails'
      end

      it 'should return raise an exception after 5 retries.' do
        helper = described_class.new(node)
        expect(Chef::HTTP).to receive(:new).with(external_url).exactly(5).times.and_return(client)
        expect(client).to receive(:get).with('').exactly(5).times.and_raise('Bad connection')
        allow(helper).to receive(:sleep).and_return(0)
        expect { helper.elastic_search_major_version }.to raise_error(/Failed to connect/)
      end
    end

    context 'when elastic search is version 2' do
      it 'should return 2' do
        helper = described_class.new(node)
        expect(Chef::HTTP).to receive(:new).with(external_url).and_return(client)
        expect(client).to receive(:get).with('').and_return(response)
        expect(helper.elastic_search_major_version).to eq(2)
      end
    end

    context 'when elastic search is version 5' do
      let(:elastic_version) { '5.0.1' }

      it 'should return 5' do
        helper = described_class.new(node)
        expect(Chef::HTTP).to receive(:new).with(external_url).and_return(client)
        expect(client).to receive(:get).with('').and_return(response)
        expect(helper.elastic_search_major_version).to eq(5)
      end
    end

    context 'when elastic search is version unsupported' do
      let(:elastic_version) { '200.50.35' }

      it 'should raise an exception' do
        helper = described_class.new(node)
        expect(Chef::HTTP).to receive(:new).with(external_url).and_return(client)
        expect(client).to receive(:get).with('').and_return(response)
        expect { helper.elastic_search_major_version }.to raise_error(/Unsupported elasticsearch version/)
      end
    end

    context 'when elastic search response is garbled' do
      let(:response) do
        <<-EOS
          {
            "name": "L33tHaxors",
            "version": {
              "some": "completenonsense"
            },
            "tagline": "You Know, for Lulz"
          }
        EOS
      end

      it 'should raise an exception' do
        helper = described_class.new(node)
        expect(Chef::HTTP).to receive(:new).with(external_url).and_return(client)
        expect(client).to receive(:get).with('').and_return(response)
        expect { helper.elastic_search_major_version }.to raise_error(/Unable to parse elasticsearch response/)
      end
    end

    context 'when solr is external' do
      let(:provider) { 'solr' }

      it 'should return 0' do
        helper = described_class.new(node)
        expect(helper.elastic_search_major_version).to eq(0)
      end
    end

    context 'when solr is internal' do
      let(:external) { false }
      let(:external_url) { nil }
      let(:provider) { 'solr' }

      it 'should return a default version 0' do
        helper = described_class.new(node)
        expect(helper.elastic_search_major_version).to eq(0)
      end
    end
  end
end
