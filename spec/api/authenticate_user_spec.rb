# -*- coding: utf-8 -*-
require 'pedant/rspec/common'

describe 'authenticate_user' do
  def self.ruby?
    true
  end

  let (:username) { platform.non_admin_user.name }
  let (:password) { 'foobar' }
  let (:request_url) { "#{platform.server}/authenticate_user" }

  let (:body) { { 'username' => username, 'password' => password } }

  context 'GET /authenticate_user' do

    it 'returns 404 ("Not Found") for superuser' do
      get(request_url, superuser).should look_like({
          :status => 404
        })
    end

    it 'returns 404 ("Not Found") for admin/different user' do
      get(request_url, platform.admin_user).should look_like({
          :status => 404
        })
    end

    it 'returns 404 ("Not Found") for non-admin/same user' do
      get(request_url, platform.non_admin_user).should look_like({
          :status => 404
        })
    end

    it 'returns 404 ("Not Found") for invalid user' do
      get(request_url, invalid_user).should look_like({
          :status => 404
        })
    end
      
  end # 'GET /authenticate_user'

  context 'PUT /authenticate_user' do
      
    it 'returns 404 ("Not Found") for superuser' do
      put(request_url, superuser, :payload => body).should look_like({
          :status => 404
        })
    end

    it 'returns 404 ("Not Found") for admin/different user' do
      put(request_url, platform.admin_user, :payload => body).should look_like({
          :status => 404
        })
    end

    it 'returns 404 ("Not Found") for non-admin/same user' do
      put(request_url, platform.non_admin_user, :payload => body).should look_like({
          :status => 404
        })
    end

    it 'returns 404 ("Not Found") for invalid user' do
      put(request_url, invalid_user, :payload => body).should look_like({
          :status => 404
        })
    end

  end # 'PUT /authenticate_user'

  context 'POST /authenticate_user' do

    context 'with correct credentials', :smoke do
      it 'superuser user returns 200 ("OK")' do
        post(request_url, superuser, :payload => body).should look_like({
            :status => 200
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
            :status => 200
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
          :status => 404
        })
    end

    it 'returns 404 ("Not Found") for admin/different user' do
      delete(request_url, platform.admin_user).should look_like({
          :status => 404
        })
    end

    it 'returns 404 ("Not Found") for non-admin/same user' do
      delete(request_url, platform.non_admin_user).should look_like({
          :status => 404
        })
    end

    it 'returns 404 ("Not Found") for invalid user' do
      delete(request_url, invalid_user).should look_like({
          :status => 404
        })
    end
      
  end # 'DELETE /authenticate_user'
end # describe 'authenticate_user'
