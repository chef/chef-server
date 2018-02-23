def wait_for_http_listener(uri_string, retries = 10)
  require 'net/http'
  require 'openssl'
  require 'uri'
  uri = URI(uri_string)
  begin
    puts "Trying GET #{uri_string}"
    Net::HTTP.start(uri.host, uri.port,
                    use_ssl: true,
                    ssl_version: Pedant::Config.ssl_version, # ??
                    verify_mode: OpenSSL::SSL::VERIFY_NONE) do |http|
      request = Net::HTTP::Get.new uri
      http.request request
    end
    puts "Succesfully made GET to #{uri_string}"
  rescue StandardError => e
    if retries <= 0
      raise e
    else
      puts "GET #{uri_string} failed: #{e} retrying in 0.1 seconds (#{retries} retries remaining)"
      retries -= 1
      sleep 0.1
      retry
    end
  end
end

def start_compliance_stub(port)
  # Start stub compliance server in a thread
  require 'webrick'
  require 'webrick/https'
  require 'openssl'
  require 'thread'
  Thread.new do
    server = WEBrick::HTTPServer.new(:Port => port,
                                     :SSLEnable => true,
                                     :SSLVerifyClient => OpenSSL::SSL::VERIFY_NONE,
                                     :SSLCertName => [%w[CN localhost]])
    server.mount_proc '/' do |req, res|
      case req.path
      when %r{/compliance/profiles/([^/]+)/?}
        res.body = '{ "response": "ok" }'
      when %r{/compliance/profiles/([^/]+)/([^/]+)/?}
        res.body = '{ "response": "ok" }'
      else
        res.body = '{}'
        res.status = 404
      end
    end
    server.start
  end
end

describe "Compliance Proxy Tests", :compliance_proxy_tests do
  if !Pedant.config.compliance_proxy_tests
    it "Compliance Proxy Tests disabled since config.compliance_proxy_tests = false" do
      expect(true).to eq(true)
    end
  else
    before(:all) do
      port = Pedant.config.compliance_proxy_port
      puts "Starting stub compliance server on #{port}"
      start_compliance_stub(port)
      puts "Waiting for listener on #{port}"
      wait_for_http_listener("https://localhost:#{port}/compliance/profiles/conn_test")
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
