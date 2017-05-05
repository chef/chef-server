require 'pedant/rspec/common'
require 'openssl'

describe "oc_id API", :oc_id do
  context "status endpoint" do
    let(:request_url) { "#{platform.server}/id/v1/status" }
    let(:good_status) do
      {"status" => "ok",
       "erchef" => { "status" => "reachable" },
       "postgres"=>{ "status" => "reachable" }}
    end

    context "GET /id/v1/status" do
      it "retuns 200" do
        get(request_url, platform.superuser).should look_like({:status => 200,
                                                               :body => good_status})
      end
    end
  end

  let(:username) {  "pedant-oc-test-user-#{rand(10**7...10**8).to_s}" }

  let(:password) { "foobar" } #hardcoded at the platform layer

  # create user before each test so that we don't regen the keys
  # of the standard user used in other tests.
  let(:reset_user) {
    platform.create_user(username)
  }

  # delete user after each test
  after(:each) do
    platform.delete_user(reset_user)
  end

  context "key reset" do

    let(:request_headers) do
      {
        "Content-Type" => "application/x-www-form-urlencoded",
        "Cookie" => csrf[:cookie]
      }
    end

    let(:csrf) do
      response = get("#{platform.server}/id/signin", reset_user, headers: {"Accept" => "text/html"})
      cookie = response.headers[:set_cookie][1].split(";").first
      # I KNOW. I'll leave it up to reviewers whether we should pull
      # in nokogiri or hpricot just do to this
      re = /<meta name="csrf-token" content="(.*)" \/>/
      token = response.match(re)[1]
      headers = { "Content-Type" => "application/x-www-form-urlencoded", "Cookie" => cookie }
      body = "username=#{username}&password=#{password}&authenticity_token=#{CGI.escape(token)}&commit=Sign+In"
      # Now sign in
      response = post("#{platform.server}/id/auth/chef/callback", reset_user, headers: headers, payload: body)
      cookie = response.headers[:set_cookie][1].split(";").first
      { cookie: cookie, token: token}
    end

    let(:request_url) { "#{platform.server}/id/profile/regen_key" }
    let(:request_body) { "authenticity_token=#{CGI.escape(csrf[:token])}&commit=Get+a+New+Key" }
    let(:response) { post(request_url, reset_user, headers: request_headers, payload: request_body) }
    it "returns a non-zero file" do
      expect(response.code).to eq(200)
      expect(response.body.length).to_not eq(0)
    end

    it "returns a valid key" do
      expect(response.code).to eq(200)
      # This will raise if it isn't a valid key
      expect(OpenSSL::PKey::RSA.new(response.body).class).to eq(OpenSSL::PKey::RSA)
      expect(response.body).to_not eq(reset_user.signing_key.to_s)
    end
  end

  context "signin" do
    let(:request_url) { "#{platform.server}/id/auth/chef/callback" }
    let(:request_body) { "username=#{username}&password=#{password}&authenticity_token=#{CGI.escape(csrf[:token])}&commit=Sign+In" }
    let(:request_headers) do
      {
        "Content-Type" => "application/x-www-form-urlencoded",
        "Cookie" => csrf[:cookie]
      }
    end

    let(:csrf) do
      response = get("#{platform.server}/id/signin", platform.superuser, headers: {"Accept" => "text/html"})
      cookie = response.headers[:set_cookie][1].split(";").first
      # I KNOW. I'll leave it up to reviewers whether we should pull
      # in nokogiri or hpricot just do to this
      re = /<meta name="csrf-token" content="(.*)" \/>/
      token = response.match(re)[1]
      { cookie: cookie, token: token}
    end

    let(:response) { post(request_url, reset_user, headers: request_headers, payload: request_body) }

    context "with correct password" do
      let(:password) { "foobar" } #hardcoded at the platform layer
      context "POST /id/auth/chef/callback" do
        it "redirects us to authorized applications" do
          expect(response.code).to eq(302)
          expect(response.headers[:location]).to match(%r{/id/oauth/authorized_applications})
        end
      end
    end

    context "with incorrect password" do
      let(:password) { "WRONGWRONGWRONG" }
      context "POST /id/auth/chef/callback" do
        it "redirects us to an error" do
          expect(response.code).to eq(302)
          expect(response.headers[:location]).to match(%r{/id/auth/failure\?message=invalid_credentials&strategy=chef})
        end
      end
    end
  end
end
