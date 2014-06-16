# -*- coding: utf-8 -*-
require 'pedant/rspec/common'
require 'json'

describe 'authenticate_user' do
  def self.ruby?
    true
  end

  def invalid_verb_response_code
    ruby? ? 404 : 405
  end

  let (:username) {
    if platform.ldap_testing
      platform.ldap[:account_name]
    else
      platform.non_admin_user.name
    end
  }

  let (:password) {
    if platform.ldap_testing
      platform.ldap[:account_password]
    else
      'foobar'
    end
  }

  let (:request_url) { "#{platform.server}/authenticate_user" }
  let (:body) { { 'username' => username, 'password' => password } }
  let (:response_body) do
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
          'email' => platform.non_admin_user.name + "@opscode.com",
          'username' => platform.non_admin_user.name
        }
      }
    end
  end

  let (:authentication_error_msg) {
    "Failed to authenticate: Username and password incorrect"
  }

  context 'GET /authenticate_user' do

    # We'd just loop this, but unfortunately, superuser et al aren't available outside
    # of test scope, so easier just to do multiple similar tests

    it 'returns 404 ("Not Found") for superuser' do
      get(request_url, superuser).should look_like({
          :status => invalid_verb_response_code
        })
    end

    it 'returns 404 ("Not Found") for admin/different user' do
      get(request_url, platform.admin_user).should look_like({
          :status => invalid_verb_response_code
        })
    end

    it 'returns 404 ("Not Found") for non-admin/same user' do
      get(request_url, platform.non_admin_user).should look_like({
          :status => invalid_verb_response_code
        })
    end

    it 'returns 404 ("Not Found") for invalid user' do
      get(request_url, invalid_user).should look_like({
          :status => invalid_verb_response_code
        })
    end

  end # 'GET /authenticate_user'

  context 'PUT /authenticate_user' do

    it 'returns 404 ("Not Found") for superuser' do
      put(request_url, superuser, :payload => body).should look_like({
          :status => invalid_verb_response_code
        })
    end

    it 'returns 404 ("Not Found") for admin/different user' do
      put(request_url, platform.admin_user, :payload => body).should look_like({
          :status => invalid_verb_response_code
        })
    end

    it 'returns 404 ("Not Found") for non-admin/same user' do
      put(request_url, platform.non_admin_user, :payload => body).should look_like({
          :status => invalid_verb_response_code
        })
    end

    it 'returns 404 ("Not Found") for invalid user' do
      put(request_url, invalid_user, :payload => body).should look_like({
          :status => invalid_verb_response_code
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

      it 'admin/different user returns 403 ("Forbidden")' do
        # This doubles as a test of a non-superuser checking a different user
        post(request_url, platform.admin_user, :payload => body).should look_like({
            :status => 403
          })
      end

      it 'non-admin/same user returns 403 ("Forbidden")' do
        # This doubles as a test of a the same non-superuser checking themselves
        post(request_url, platform.non_admin_user, :payload => body).should look_like({
            :status => 403
          })
      end

      it 'invalid user returns 401 ("Unauthorized")' do
        post(request_url, invalid_user, :payload => body).should look_like({
            :status => 401
          })
      end
    end

    context 'with invalid username' do

      let (:username) { "kneelbeforezod" }

      it 'superuser returns 401 ("Unauthorized")', :smoke do
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

      it 'admin/different user returns 403 ("Forbidden")' do
        post(request_url, platform.admin_user, :payload => body).should look_like({
            :status => 403
          })
      end

      it 'non-admin/same user returns 403 ("Forbidden")' do
        post(request_url, platform.non_admin_user, :payload => body).should look_like({
            :status => 403
          })
      end
    end

    context 'with incorrect password' do

      let (:password) { "badger badger" }

      it 'superuser returns 401 ("Unauthorized")', :smoke do
        post(request_url, superuser, :payload => body).should look_like({
            :status => 401
          })
      end

      it 'admin/different user returns 403 ("Forbidden")' do
        post(request_url, platform.admin_user, :payload => body).should look_like({
            :status => 403
          })
      end

      it 'non-admin/same user returns 403 ("Forbidden")' do
        post(request_url, platform.non_admin_user, :payload => body).should look_like({
            :status => 403
          })
      end
    end

    # Again, we'd just loop a lot of these, but unfortunately, superuser et al
    # aren't available outside of test scope, so easier just to do multiple similar
    # tests

    context 'with missing username' do

      let (:body) { { 'password' => password } }

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

    context 'with missing password' do

      let (:body) { { 'username' => username } }

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

    context 'with empty username' do
      let (:username) { "" }

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

    context 'with empty password' do
      let (:password) { "" }

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

    context 'with username = user' do

      let (:body) { { 'user' => username, 'password' => password } }

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

    context 'with password = pass' do

      let (:body) { { 'username' => username, 'pass' => password } }

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

    context 'with empty body' do

      let (:body) { {} }

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

    context 'with no body' do
      it 'superuser returns 400 ("Bad Request")' do
        pending "returns 200 instead" do
          # TODO: Why???
          post(request_url, superuser, :payload => body).should look_like({
              :status => 400
            })
        end
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

      it 'invalid user returns 401 ("Unauthorized")' do
        post(request_url, invalid_user).should look_like({
            :status => 401
          })
      end
    end

    context 'with extra junk in body' do
      # Not sure we should actually ignore extra junk, but, well, meh

      let (:body) { { 'username' => username, 'password' => password,
          'junk' => 'extra' } }

      it 'superuser returns 200 ("Ok")' do
        post(request_url, superuser, :payload => body).should look_like({
            :status => 200,
            :body_exact => response_body
          })
      end

      it 'admin/different user returns 403 ("Forbidden")' do
        post(request_url, platform.admin_user, :payload => body).should look_like({
            :status => 403
          })
      end

      it 'non-admin/same user returns 403 ("Forbidden")' do
        post(request_url, platform.non_admin_user, :payload => body).should look_like({
            :status => 403
          })
      end

      it 'invalid user returns 401 ("Unauthorized")' do
        post(request_url, invalid_user, :payload => body).should look_like({
            :status => 401
          })
      end
    end
  end # context 'POST /authenticate_user'

  context 'DELETE /authenticate_user' do
    # This should do nothing0

    it 'returns 404 ("Not Found") for superuser' do
      delete(request_url, superuser).should look_like({
          :status => invalid_verb_response_code
        })
    end

    it 'returns 404 ("Not Found") for admin/different user' do
      delete(request_url, platform.admin_user).should look_like({
          :status => invalid_verb_response_code
        })
    end

    it 'returns 404 ("Not Found") for non-admin/same user' do
      delete(request_url, platform.non_admin_user).should look_like({
          :status => invalid_verb_response_code
        })
    end

    it 'returns 404 ("Not Found") for invalid user' do
      delete(request_url, invalid_user).should look_like({
          :status => invalid_verb_response_code
        })
    end

  end # 'DELETE /authenticate_user'
end # describe 'authenticate_user'
