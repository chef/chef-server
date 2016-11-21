require_relative '../../libraries/chef_backend.rb'

describe ChefBackend do
  describe "#configured_members" do
    it "Returns a hash of name => IP based on the chef_backend_members config" do
      node = {"private_chef" => {"chef_backend_members" => ["1.2.3.4", "2.3.4.5"]}}
      expect(ChefBackend.configured_members(node)).to eq({
                                                           "backend0" => "1.2.3.4",
                                                           "backend1" => "2.3.4.5"
                                                         })
    end
  end

  describe "#etcd_members" do
    let(:http_client) { double("Chef::HTTP") }
    let(:members_response) do
      <<EOF
{"members":[{"id":"2f74e8c0e2bda03","name":"4ba4ae0a6686cf6b67590e621459f1e2","peerURLs":["http://192.168.33.216:2380"],"clientURLs":["http://192.168.33.216:2379"]},{"id":"9a7046eca5b7429b","name":"89cc3a0c2e3e51bdc5db1f97f63deb99","peerURLs":["http://192.168.33.217:2380"],"clientURLs":["http://192.168.33.217:2379"]},{"id":"c5a8c92212f542bb","name":"7caffa2c440aa682a2abbf4902d35fbd","peerURLs":["http://192.168.33.215:2380"],"clientURLs":["http://192.168.33.215:2379"]}]}
EOF
    end

    let(:parsed_response) do
    {
      "4ba4ae0a6686cf6b67590e621459f1e2" => "192.168.33.216",
      "89cc3a0c2e3e51bdc5db1f97f63deb99" => "192.168.33.217",
      "7caffa2c440aa682a2abbf4902d35fbd" => "192.168.33.215"
    }
    end

    it "makes a request /v2/members on the give host and port" do
      expect(Chef::HTTP).to receive(:new).with("http://1.2.3.4:99").and_return(http_client)
      expect(http_client).to receive(:get).with("/v2/members").and_return(members_response)
      ChefBackend.etcd_members("1.2.3.4", 99)
    end

    it "parses the raw response, returning a hash of name => ip" do
      expect(Chef::HTTP).to receive(:new).with("http://1.2.3.4:99").and_return(http_client)
      expect(http_client).to receive(:get).with("/v2/members").and_return(members_response)
      expect(ChefBackend.etcd_members("1.2.3.4", 99)).to eq(parsed_response)
    end
  end
end
