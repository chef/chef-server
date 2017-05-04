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

    context "/organizations/ORG/owners/OWNER/compliance[/PROFILE]" do
      let(:good_status) { {"response" => "ok"} }

      context "GET /organizations/ORG/owners/OWNER/compliance" do
        let(:request_url) { api_url("owners/foobar/compliance") }
          it "retuns 200" do
            get(request_url, admin_user).should look_like({:status => 200,
                                                           :body => good_status})
          end
      end

      context "GET /organizations/ORG/owners/OWNER/compliance/PROFILE" do
        let(:request_url) { api_url("owners/foobar/compliance/testprofile") }
        it "retuns 200" do
          puts request_url
          get(request_url, admin_user).should look_like({:status => 200,
                                                         :body => good_status})
        end
      end

      context "GET /compliance/organizations/ORG/owners/OWNER/compliance" do
        let(:request_url) { "#{platform.server}/compliance/organizations/#{platform.test_org.name}/owners/foobar/compliance" }
        it "retuns 200" do
          puts request_url
          get(request_url, admin_user).should look_like({:status => 200,
                                                         :body => good_status})
        end
      end

      context "GET /compliance/organizations/ORG/owners/OWNER/compliance/PROFILE" do
        let(:request_url) { "#{platform.server}/compliance/organizations/#{platform.test_org.name}/owners/foobar/compliance/testprofile" }
        it "retuns 200" do
          get(request_url, admin_user).should look_like({:status => 200,
                                                         :body => good_status})
        end
      end
    end
  end
end
