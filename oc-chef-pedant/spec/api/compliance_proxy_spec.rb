describe "Compliance Proxy Tests", :compliance_proxy_tests do
  if !Pedant.config.compliance_proxy_tests
    it "Compliance Proxy Tests disabled since config.compliance_proxy_tests = false" do
      expect(true).to eq(true)
    end
  else
    before(:all) do
      # Start stub compliance server in a thread
      require 'webrick'
      require 'webrick/https'
      require 'openssl'
      require 'thread'
      cert_name = [
        %w[CN localhost],
      ]
      puts "Starting stub compliance server on #{Pedant.config.compliance_proxy_port}"
      Thread.new do
        server = WEBrick::HTTPServer.new(:Port => Pedant.config.compliance_proxy_port,
                                         :SSLEnable => true,
                                         :SSLCertName => cert_name)
        server.mount_proc '/' do |req, res|
          if req.path =~ %r{/compliance/profiles/([^/]+)/?}
            res.body = '{ "response": "ok" }'
          elsif req.path =~ %r{/compliance/profiles/([^/]+)/([^/]+)/?}
            res.body = '{ "response": "ok" }'
          else
            res.body = '{}'
            res.status = 404
          end
        end
        server.start
      end
    end

    let(:response) { get(request_url, admin_user) }

    context "GET /organizations/ORG/owners/OWNER/compliance" do
      let(:request_url) { api_url("owners/foobar/compliance") }
      it "returns 200" do
          expect(response.code).to eq(200)
      end
    end

    context "GET /organizations/ORG/owners/OWNER/compliance/PROFILE" do
      let(:request_url) { api_url("owners/foobar/compliance/testprofile") }
      it "returns 200" do
        expect(response.code).to eq(200)
      end
    end

    context "GET /compliance/organizations/ORG/owners/OWNER/compliance" do
      let(:request_url) { "#{platform.server}/compliance/organizations/#{platform.test_org.name}/owners/foobar/compliance" }
      it "returns 200" do
        expect(response.code).to eq(200)
      end
    end

    context "GET /compliance/organizations/ORG/owners/OWNER/compliance/PROFILE" do
      let(:request_url) { "#{platform.server}/compliance/organizations/#{platform.test_org.name}/owners/foobar/compliance/testprofile" }
      it "returns 200" do
        expect(response.code).to eq(200)
      end
    end
  end
end
