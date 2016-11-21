# -*- coding: utf-8 -*-
require 'pedant/rspec/common'
require 'json'

describe 'authenticate_user', :users do
  let(:username) {
    if platform.ldap_testing
      platform.ldap[:account_name]
    else
      platform.non_admin_user.name
    end
  }

  let(:password) {
    if platform.ldap_testing
      platform.ldap[:account_password]
    else
      'foobar'
    end
  }

  let(:request_url) { "#{platform.server}/authenticate_user" }
  let(:body) { { 'username' => username, 'password' => password } }
  let(:response_body) do
    if platform.ldap_testing
      {
        'status' => platform.ldap[:status],
        'user' => {
          'first_name' => platform.ldap[:first_name],
          'last_name' => platform.ldap[:last_name],
          'display_name' => platform.ldap[:display_name],
          'email' => platform.ldap[:email],
          'username' => platform.ldap[:account_name],
          'city' => platform.ldap[:city],
          'country' => platform.ldap[:country],
          'external_authentication_uid' => platform.ldap[:account_name],
          'recovery_authentication_enabled' => platform.ldap[:recovery_authentication_enabled],
        }
      }
    else
      {
        'status' => 'linked',
        'user' => {
          'first_name' => platform.non_admin_user.name,
          'last_name' => platform.non_admin_user.name,
          'display_name' => platform.non_admin_user.name,
          'email' => platform.non_admin_user.name + "@chef.io",
          'username' => platform.non_admin_user.name
        }
      }
    end
  end

  let(:body) { { 'username' => username, 'password' => password } }
  let(:response_body) { {
      'status' => 'linked',
      'user' => {
        'first_name' => platform.non_admin_user.name,
        'last_name' => platform.non_admin_user.name,
        'display_name' => platform.non_admin_user.name,
        'email' => platform.non_admin_user.name + "@chef.io",
        'username' => platform.non_admin_user.name
      }} }
  let(:authentication_error_msg) { ["Failed to authenticate: Username and password incorrect"] }

  context 'GET /authenticate_user' do

    # We'd just loop this, but unfortunately, superuser et al aren't available outside
    # of test scope, so easier just to do multiple similar tests

    it 'returns 405 for superuser' do
      get(request_url, superuser).should look_like({
          :status => 405
        })
    end

    it 'returns 405 for admin/different user' do
      get(request_url, platform.admin_user).should look_like({
          :status => 405
        })
    end

    it 'returns 405 for non-admin/same user' do
      get(request_url, platform.non_admin_user).should look_like({
          :status => 405
        })
    end

    it 'returns 405 for invalid user' do
      get(request_url, invalid_user).should look_like({
          :status => 405
        })
    end

  end # 'GET /authenticate_user'

  context 'PUT /authenticate_user' do

    it 'returns 405 for superuser' do
      put(request_url, superuser, :payload => body).should look_like({
          :status => 405
        })
    end

    it 'returns 405 for admin/different user' do
      put(request_url, platform.admin_user, :payload => body).should look_like({
          :status => 405
        })
    end

    it 'returns 405 for non-admin/same user' do
      put(request_url, platform.non_admin_user, :payload => body).should look_like({
          :status => 405
        })
    end

    it 'returns 405 for invalid user' do
      put(request_url, invalid_user, :payload => body).should look_like({
          :status => 405
        })
    end

  end # 'PUT /authenticate_user'

  context 'POST /authenticate_user' do

    context 'with correct credentials', :smoke do
      it 'superuser user returns 200 ("OK")' do
        post(request_url, superuser, :payload => body).should look_like({
            :status => 200,
            :body_exact => response_body
          })
      end

      it 'admin/different user returns 403 ("Forbidden")', :authorization do
        # This doubles as a test of a non-superuser checking a different user
        post(request_url, platform.admin_user, :payload => body).should look_like({
            :status => 403
          })
      end

      it 'non-admin/same user returns 403 ("Forbidden")', :authorization do
        # This doubles as a test of a the same non-superuser checking themselves
        post(request_url, platform.non_admin_user, :payload => body).should look_like({
            :status => 403
          })
      end

      it 'invalid user returns 401 ("Unauthorized")', :authorization do
        post(request_url, invalid_user, :payload => body).should look_like({
            :status => 401
          })
      end
    end

    context 'and user has external authentication enabled' do
      context 'but local bypass parameter is used' do
        let(:password) { "badger badger" }
        let(:username) { "test-#{Time.now.to_i}-#{Process.pid}" }
        let(:create_body) do
          {
            "username" => username,
            "email" => "#{username}@chef.io",
            "first_name" => username,
            "last_name" => username,
            "display_name" => username,
            "external_authentication_uid" => username,
            "password" =>  password
          }
        end
        let(:update_body) do
          {
            "username" => username,
            "display_name" => username,
            "email" => "#{username}@chef.io",
            "external_authentication_uid" => username
          }
        end
        before :each  do
          # Create our user and separately update him with ext auth uid:
          # the presence of ext auth uid in initial create will prevent
          # password from being captured.
          # NOTE: once ldap testing is integrated, this user will need to be generated
          # with a different password in ldap. Currently it is safe as there is no ldap
          # locally, and so any ldap login attempt will fail.
          post("#{platform.server}/users", platform.superuser,
            :payload => create_body).should look_like({ :status => 201 })
          put("#{platform.server}/users/#{username}", platform.superuser,
            :payload => update_body).should look_like({ :status => 200 })
        end
        after :each do
          delete("#{platform.server}/users/#{username}", platform.superuser)
        end

        it "allows authentication with correct password and no ldap" do
          local_body = body.clone
          local_body["local"]  = true
          post("#{request_url}", platform.superuser,
            :payload => local_body).should look_like({
              :status => 200
            })
        end
      end
    end
    context 'with invalid username' do

      let(:username) { "kneelbeforezod" }

      it 'superuser returns 401 ("Unauthorized")', :smoke, :authentication do
        # the exact error is very dependant on how LDAP is configured, so its hard to test
        # for something exact
        if platform.ldap_testing
          response = post(request_url, superuser, :payload => body)
          response.should look_like({
            :status => 401
          })
          /^Failed to authenticate: Could not locate a record with distinguished name/.should match(JSON.parse(response)["error"])
        else
          response = post(request_url, superuser, :payload => body)
          response.should look_like({
            :status => 401,
            :body_exact => {'error' => authentication_error_msg}
          })
        end
      end

      it 'admin/different user returns 403 ("Forbidden")', :authorization do
        post(request_url, platform.admin_user, :payload => body).should look_like({
            :status => 403
          })
      end

      it 'non-admin/same user returns 403 ("Forbidden")', :authorization do
        post(request_url, platform.non_admin_user, :payload => body).should look_like({
            :status => 403
          })
      end
    end

    context 'with incorrect password' do

      let(:password) { "badger badger" }

      it 'superuser returns 401 ("Unauthorized")', :smoke, :authentication do
        post(request_url, superuser, :payload => body).should look_like({
            :status => 401
          })
      end

      it 'admin/different user returns 403 ("Forbidden")', :authorization do
        post(request_url, platform.admin_user, :payload => body).should look_like({
            :status => 403
          })
      end

      it 'non-admin/same user returns 403 ("Forbidden")', :authorization do
        post(request_url, platform.non_admin_user, :payload => body).should look_like({
            :status => 403
          })
      end
    end

    # Again, we'd just loop a lot of these, but unfortunately, superuser et al
    # aren't available outside of test scope, so easier just to do multiple similar
    # tests

    context 'with missing username', :validation do

      let(:body) { { 'password' => password } }

      it 'superuser returns 400 ("Bad Request")' do
        post(request_url, superuser, :payload => body).should look_like({
            :status => 400
          })
      end

      it 'admin/different user returns 400 ("Bad Request")' do
        post(request_url, platform.admin_user, :payload => body).should look_like({
            :status => 400
          })
      end

      it 'non-admin/same user returns 400 ("Bad Request")' do
        post(request_url, platform.non_admin_user, :payload => body).should look_like({
            :status => 400
          })
      end
    end

    context 'with missing password', :validation do

      let(:body) { { 'username' => username } }

      it 'superuser returns 400 ("Bad Request")' do
        post(request_url, superuser, :payload => body).should look_like({
            :status => 400
          })
      end

      it 'admin/different user returns 400 ("Bad Request")' do
        post(request_url, platform.admin_user, :payload => body).should look_like({
            :status => 400
          })
      end

      it 'non-admin/same user returns 400 ("Bad Request")' do
        post(request_url, platform.non_admin_user, :payload => body).should look_like({
            :status => 400
          })
      end
    end

    context 'with empty username', :validation do
      let(:username) { "" }

      it 'superuser returns 400 ("Bad Request")' do
        post(request_url, superuser, :payload => body).should look_like({
            :status => 400
          })
      end

      it 'admin/different user returns 400 ("Bad Request")' do
        post(request_url, platform.admin_user, :payload => body).should look_like({
            :status => 400
          })
      end

      it 'non-admin/same user returns 400 ("Bad Request")' do
        post(request_url, platform.non_admin_user, :payload => body).should look_like({
            :status => 400
          })
      end
    end

    context 'with empty password', :validation do
      let(:password) { "" }

      it 'superuser returns 400 ("Bad Request")' do
        post(request_url, superuser, :payload => body).should look_like({
            :status => 400
          })
      end

      it 'admin/different user returns 400 ("Bad Request")' do
        post(request_url, platform.admin_user, :payload => body).should look_like({
            :status => 400
          })
      end

      it 'non-admin/same user returns 400 ("Bad Request")' do
        post(request_url, platform.non_admin_user, :payload => body).should look_like({
            :status => 400
          })
      end
    end

    context 'with username = user', :validation do

      let(:body) { { 'user' => username, 'password' => password } }

      it 'superuser returns 400 ("Bad Request")' do
        post(request_url, superuser, :payload => body).should look_like({
            :status => 400
          })
      end

      it 'admin/different user returns 400 ("Bad Request")' do
        post(request_url, platform.admin_user, :payload => body).should look_like({
            :status => 400
          })
      end

      it 'non-admin/same user returns 400 ("Bad Request")' do
        post(request_url, platform.non_admin_user, :payload => body).should look_like({
            :status => 400
          })
      end
    end

    context 'with password = pass', :validation do

      let(:body) { { 'username' => username, 'pass' => password } }

      it 'superuser returns 400 ("Bad Request")' do
        post(request_url, superuser, :payload => body).should look_like({
            :status => 400
          })
      end

      it 'admin/different user returns 400 ("Bad Request")' do
        post(request_url, platform.admin_user, :payload => body).should look_like({
            :status => 400
          })
      end

      it 'non-admin/same user returns 400 ("Bad Request")' do
        post(request_url, platform.non_admin_user, :payload => body).should look_like({
            :status => 400
          })
      end
    end

    context 'with empty body', :validation do

      let(:body) { {} }

      it 'superuser returns 400 ("Bad Request")' do
        post(request_url, superuser, :payload => body).should look_like({
            :status => 400
          })
      end

      it 'admin/different user returns 400 ("Bad Request")' do
        post(request_url, platform.admin_user, :payload => body).should look_like({
            :status => 400
          })
      end

      it 'non-admin/same user returns 400 ("Bad Request")' do
        post(request_url, platform.non_admin_user, :payload => body).should look_like({
            :status => 400
          })
      end
    end

    context 'with no body', :validation do
      it 'superuser returns 400 ("Bad Request")' do
          post(request_url, superuser).should look_like({
              :status => 400
            })
      end

      it 'admin/different user returns 400 ("Bad Request")' do
        post(request_url, platform.admin_user).should look_like({
            :status => 400
          })
      end

      it 'non-admin/same user returns 400 ("Bad Request")' do
        post(request_url, platform.non_admin_user).should look_like({
            :status => 400
          })
      end

      it 'invalid user returns  400 ("Bad Request")', :validation do
        post(request_url, invalid_user).should look_like({
            :status => 400
          })
      end
    end

    context 'with extra junk in body' do
      # Not sure we should actually ignore extra junk, but, well, meh

      let(:body) { { 'username' => username, 'password' => password,
          'junk' => 'extra' } }

      it 'superuser returns 200 ("Ok")' do
        post(request_url, superuser, :payload => body).should look_like({
            :status => 200,
            :body_exact => response_body
          })
      end

      it 'admin/different user returns 403 ("Forbidden")', :authorization do
        post(request_url, platform.admin_user, :payload => body).should look_like({
            :status => 403
          })
      end

      it 'non-admin/same user returns 403 ("Forbidden")', :authorization do
        post(request_url, platform.non_admin_user, :payload => body).should look_like({
            :status => 403
          })
      end

      it 'invalid user returns 401 ("Unauthorized")', :authentication do
        post(request_url, invalid_user, :payload => body).should look_like({
            :status => 401
          })
      end
    end


    let(:request_url) { "#{platform.server}/authenticate_user" }

    context "when the webui superuser is specified as the target user" do
      let(:requestor) { superuser }

      let(:request_body) do
        {
          username: superuser.name,
          password: "DOES_NOT_MATTER_FOR_TEST",
        }
      end

      it "should return Forbidden", :authorization do
        post(request_url, superuser, :payload => request_body).should look_like(
          :status => 403
        )
      end
    end # context when the webui superuser is specified as the user
  end # context 'POST /authenticate_user'

  context 'DELETE /authenticate_user' do
    # This should do nothing0

    it 'returns 405 for superuser' do
      delete(request_url, superuser).should look_like({
          :status => 405
        })
    end

    it 'returns 405 for admin/different user' do
      delete(request_url, platform.admin_user).should look_like({
          :status => 405
        })
    end

    it 'returns 405 for non-admin/same user' do
      delete(request_url, platform.non_admin_user).should look_like({
          :status => 405
        })
    end

    it 'returns 405 for invalid user' do
      delete(request_url, invalid_user).should look_like({
          :status => 405
        })
    end

  end # 'DELETE /authenticate_user'

end # describe 'authenticate_user'
