require 'pedant/rspec/common'
require 'openssl'

describe "oc_id API", :oc_id do

  let(:request_headers) do
    {
      "Content-Type" => "application/x-www-form-urlencoded",
      "Accept" => "text/html",
      "Cookie" => csrf[:cookie]
    }
  end

  let(:username) {  "pedant-oc-test-user-#{rand(10**7...10**8).to_s}" }
  let(:password) { "foobar" } # hardcoded at the platform layer
  let(:user_overrides) do
    {
      :overrides => {
        "first_name" => "#{username}-first-name",
        "middle_name" => "#{username}-middle-name",
        "last_name" => "#{username}-last-name",
        "display_name" => "#{username}-display-name",
        "email" => "#{username}-email@host.com",
      }
    }
  end

  # create user before each test so that we don't regen the keys
  # of the standard user used in other tests.
  let(:oc_id_user) { platform.create_user(username, user_overrides) }

  # delete user after each test
  after(:each) do
    platform.delete_user(oc_id_user)
  end

  context "status endpoint" do
    let(:request_url) { "#{platform.server}/id/v1/status" }
    let(:good_status) do
      {"status" => "ok",
       "erchef" => { "status" => "reachable" },
       "postgres"=>{ "status" => "reachable" }}
    end

    context "GET /id/v1/status" do
      it "returns 200" do
        get(request_url, platform.superuser).should look_like({:status => 200,
                                                               :body => good_status})
      end
    end
  end

  def signin(with_callback = true)
    response = get("#{platform.server}/id/signin", oc_id_user, headers: {"Accept" => "text/html"})
    cookie = response.headers[:set_cookie][1].split(";").first
    # I KNOW. I'll leave it up to reviewers whether we should pull
    # in nokogiri or hpricot just do to this
    re = /<meta name="csrf-token" content="(.*)" \/>/
    token = response.match(re)[1]
    headers = { "Content-Type" => "application/x-www-form-urlencoded", "Cookie" => cookie }
    body = "username=#{username}&password=#{password}&authenticity_token=#{CGI.escape(token)}&commit=Sign+In"
    # Now sign in
    if with_callback
      response = post("#{platform.server}/id/auth/chef/callback", oc_id_user, headers: headers, payload: body)
      cookie = response.headers[:set_cookie][1].split(";").first
    end
    { cookie: cookie, token: token}
  end

  context "key reset" do

    let(:csrf) { signin }
    let(:request_url) { "#{platform.server}/id/profile/regen_key" }
    let(:request_body) { "authenticity_token=#{CGI.escape(csrf[:token])}&commit=Get+a+New+Key" }
    let(:response) { post(request_url, oc_id_user, headers: request_headers, payload: request_body) }

    it "returns a non-zero file" do
      expect(response.code).to eq(200)
      expect(response.body.length).to_not eq(0)
    end

    it "returns a valid key" do
      expect(response.code).to eq(200)
      # This will raise if it isn't a valid key
      expect(OpenSSL::PKey::RSA.new(response.body).class).to eq(OpenSSL::PKey::RSA)
      expect(response.body).to_not eq(oc_id_user.signing_key.to_s)
    end
  end

  context "signin" do
    let(:request_url) { "#{platform.server}/id/auth/chef/callback" }
    let(:request_body) { "username=#{username}&password=#{password}&authenticity_token=#{CGI.escape(csrf[:token])}&commit=Sign+In" }
    let(:csrf) { signin(false) }
    let(:response) { post(request_url, oc_id_user, headers: request_headers, payload: request_body) }

    context "with correct password" do
      let(:password) { "foobar" } # hardcoded at the platform layer

      context "POST /id/auth/chef/callback" do
        it "redirects us to authorized applications" do
          expect(response.code).to eq(302)
          expect(response.headers[:location]).to end_with('/id/oauth/authorized_applications')
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

  context "/id/profile#show" do
    let(:csrf) { signin }
    let(:request_url) { "#{platform.server}/id/profile" }
    let(:request_body) { "authenticity_token=#{CGI.escape(csrf[:token])}" }
    let(:response) { get(request_url, oc_id_user, headers: request_headers, payload: request_body) }

    it "shows the profile" do
      expect(response.code).to eq(200)
      expect(response.body).to match(%r{Signed in as: #{username}})
      expect(response.body).to match(/<input[^>]*value="#{username}-first-name"[^>]*id="user_first_name"\W*\/>/)
      expect(response.body).to match(/<input[^>]*value="#{username}-middle-name"[^>]*id="user_middle_name"\W*\/>/)
      expect(response.body).to match(/<input[^>]*value="#{username}-last-name"[^>]*id="user_last_name"\W*\/>/)
      expect(response.body).to match(/<input[^>]*value="#{username}-email@host\.com"[^>]*id="user_email"\W*\/>/)
    end
  end

  # NOTE 2017/06/02 sr: we don't update the email, since that makes oc_id send
  #                     an email and that's not something we'd like to do from
  #                     pedant running against real deployments
  context "/id/profile#update" do
    let(:csrf) { signin }
    let(:request_url) { "#{platform.server}/id/profile" }
    let(:request_body) { "authenticity_token=#{CGI.escape(csrf[:token])}&"\
                         "#{CGI.escape('user[first_name]')}=#{username.upcase}-FIRST-NAME&"\
                         "#{CGI.escape('user[last_name]')}=#{username.upcase}-LAST-NAME&"\
                         "#{CGI.escape('user[middle_name]')}=#{username.upcase}-MIDDLE-NAME&"\
                         "_method=PUT&"\
                         "commit=Save+Changes" }
    let(:response) { post(request_url, oc_id_user, headers: request_headers, payload: request_body) }

    # NOTE: We're using the API here because another query for profile#show
    # would, with test code laid out like this, cause another user to be created
    # and used for login -- hence we'd see that other user's profile.
    # For the same reason, everything happens in one it block :/
    it "redirects to /id/profile and updates the user record" do
      expect(response.code).to eq(302)
      expect(response.headers[:location]).to end_with('/id/profile')

      request_url = "#{platform.server}/users/#{username}"
      user_body = {
        "first_name" => "#{username.upcase}-FIRST-NAME",
        "middle_name" => "#{username.upcase}-MIDDLE-NAME",
        "last_name" => "#{username.upcase}-LAST-NAME",
        "email" => "#{username}-email@host.com", # don't want to update, see note
        # these currently can't be changed via profile update
        "display_name" => "#{username}-display-name",
        "username" => username,
        "public_key" => oc_id_user.signing_key.public_key.to_s + "\n", # What?
      }
      get(request_url, platform.superuser).should look_like({
        :status => 200,
        :body_exact => user_body
      })
    end
  end

  context "/id/profile#password" do
    let(:new_password) { "barfoo" }
    let(:csrf) { signin }
    let(:request_url) { "#{platform.server}/id/profile/password" }
    let(:request_body) { "authenticity_token=#{CGI.escape(csrf[:token])}&"\
                         "username=#{username}&"\
                         "current_password=#{password}&"\
                         "new_password=#{new_password}&"\
                         "password_confirmation=#{new_password}&"\
                         "_method=PUT" }
    let(:response) { post(request_url, oc_id_user, headers: request_headers, payload: request_body) }

    it "redirects to /id/profile and updates the user's password" do
      expect(response.code).to eq(302)
      expect(response.headers[:location]).to end_with('/id/profile')

      request_url = "#{platform.server}/authenticate_user"
      request_body = { username: username, password: new_password }
      post(request_url, platform.superuser, payload: request_body).should look_like({
        :status => 200,
        :body => { "user" => { "username" => username } },
      })
    end
  end
end
