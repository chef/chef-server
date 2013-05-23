# -*- coding: utf-8 -*-
require 'pedant/rspec/common'

describe "users", :users, :focus do
  def self.ruby?
    Pedant::Config.ruby_users_endpoint?
  end

  context "/organizations/<org>/users endpoint" do
    let(:request_url) { api_url("users") }

    context "GET /organizations/<org>/users" do
      let(:users_body) do
        [
          {"user" => {"username" => "pedant_admin_user"}},
          {"user" => {"username" => "pedant_user"}}
        ]
      end

      context "admin user" do
        it "can get org users", :smoke do
          get(request_url, platform.admin_user).should look_like({
              :status => 200,
              :body_exact => users_body
            })
        end
      end

      context "default normal user" do
        it "can get org users", :smoke do
          get(request_url, platform.non_admin_user).should look_like({
              :status => 200,
              :body_exact => users_body
            })
        end
      end

      context "default client" do
        it "returns 403" do
          get(request_url, platform.non_admin_client).should look_like({
              :status => 403
            })
        end
      end

      context "outside user" do
        it "returns 403" do
          get(request_url, outside_user).should look_like({
              :status => 403
            })
        end
      end

      context "invalid user" do
        it "returns 401" do
          get(request_url, invalid_user).should look_like({
              :status => 401
            })
        end
      end
    end # context GET /organizations/<org>/users

    context "PUT /organizations/<org>/users" do
      context "admin user" do
        # A 405 here would be fine (and is no doubt coming with erlang)
        it "returns 404 (or 405?)" do
          put(request_url, platform.admin_user).should look_like({
              :status => ruby? ? 404 : 405
            })
        end
      end
    end # context PUT /organizations/<org>/users

    context "POST /organizations/<org>/users" do
      context "admin user" do
        # A 405 here would be fine (and is no doubt coming with erlang)
        it "returns 404 (or 405?)" do
          post(request_url, platform.admin_user).should look_like({
              :status => ruby? ? 404 : 405
            })
        end
      end
    end # context POST /organizations/<org>/users

    context "DELETE /organizations/<org>/users" do
      context "admin user" do
        # A 405 here would be fine (and is no doubt coming with erlang)
        it "returns 404 (or 405?)" do
          delete(request_url, platform.admin_user).should look_like({
              :status => ruby? ? 404 : 405
            })
        end
      end
    end # context DELETE /organizations/<org>/users
  end # context /organizations/<org>/users endpoint

  context "/organizations/<org>/users/<name>" do
    let(:username) { platform.non_admin_user.name }
    let(:request_url) { api_url("users/#{username}") }

    context "GET /organizations/<org>/users/<name>" do
      let(:user_body) do
        {
          "first_name" => username,
          "last_name" => username,
          "display_name" => username,
          "email" => "#{username}@opscode.com",
          "username" => username,
          "public_key" => /^-----BEGIN PUBLIC KEY-----/
        }
      end

      context "superuser" do
        it "can get user" do
          get(request_url, platform.superuser).should look_like({
              :status => 200,
              :body_exact => user_body
            })
        end
      end

      context "admin user" do
        it "can get user", :smoke do
          get(request_url, platform.admin_user).should look_like({
              :status => 200,
              :body_exact => user_body
            })
        end
      end

      context "default normal user" do
        it "can get self", :smoke do
          get(request_url, platform.non_admin_user).should look_like({
              :status => 200,
              :body_exact => user_body
            })
        end
      end

      context "default client" do
        it "returns 403" do
          get(request_url, platform.non_admin_client).should look_like({
              :status => 403
            })
        end
      end

      context "outside user" do
        it "returns 403" do
          get(request_url, outside_user).should look_like({
              :status => 403
            })
        end
      end

      context "invalid user" do
        it "returns 401" do
          get(request_url, invalid_user).should look_like({
              :status => 401
            })
        end
      end

      context "when requesting user that doesn't exist" do
        let(:username) { "bogus" }
        it "returns 404" do
          get(request_url, platform.admin_user).should look_like({
              :status => 404
            })
        end
      end
    end # context GET /organizations/<org>/users/<name>

    context "PUT /organizations/<org>/users/<name>" do
      context "admin user" do
        # A 405 here would be fine (and is no doubt coming with erlang)
        it "returns 404 (or 405?)" do
          put(request_url, platform.admin_user).should look_like({
              :status => ruby? ? 404 : 405
            })
        end
      end
    end # context PUT /organizations/<org>/users/<name>

    context "POST /organizations/<org>/users/<name>" do
      context "admin user" do
        # A 405 here would be fine (and is no doubt coming with erlang)
        it "returns 404 (or 405?)" do
          post(request_url, platform.admin_user).should look_like({
              :status => ruby? ? 404 : 405
            })
        end
      end
    end # context POST /organizations/<org>/users/<name>

    context "DELETE /organizations/<org>/users/<name>" do
      let(:username) { "test-#{Time.now.to_i}-#{Process.pid}" }
      let(:test_user) { platform.create_user(username) }

      before :each do
        platform.associate_user_with_org(org, test_user)
        platform.add_user_to_group(org, test_user, "users")
      end

      after :each do
        delete("#{platform.server}/users/#{username}", platform.superuser)
      end

      context "admin user" do
        it "can delete user", :smoke do
          delete(request_url, platform.admin_user).should look_like({
              :status => 200
            })
          get(api_url("users"), platform.admin_user).should look_like({
              :status => 200,
              :body_exact => [
                {"user" => {"username" => "pedant_admin_user"}},
                {"user" => {"username" => "pedant_user"}}
              ]})
        end
      end

      context "non-admin user" do
        it "returns 403" do
          pending "actually returns 400" do # Wut?
            delete(request_url, platform.non_admin_user).should look_like({
                :status => 403
              })
            get(api_url("users"), platform.admin_user).should look_like({
                :status => 200,
                :body_exact => [
                  {"user" => {"username" => "pedant_admin_user"}},
                  {"user" => {"username" => "pedant_user"}},
                  {"user" => {"username" => username}}
                ]})
          end
        end
      end

      context "default client" do
        it "returns 403" do
          pending "actually returns 400" do # Wut?
            delete(request_url, platform.non_admin_client).should look_like({
                :status => 403
              })
            get(api_url("users"), platform.admin_user).should look_like({
                :status => 200,
                :body_exact => [
                  {"user" => {"username" => "pedant_admin_user"}},
                  {"user" => {"username" => "pedant_user"}},
                  {"user" => {"username" => username}}
                ]})
          end
        end
      end

      context "when user doesn't exist" do
        let(:request_url) { api_url("users/bogus") }
        it "returns 404" do
          delete(request_url, platform.non_admin_client).should look_like({
              :status => 404
            })
          get(api_url("users"), platform.admin_user).should look_like({
              :status => 200,
              :body_exact => [
                {"user" => {"username" => "pedant_admin_user"}},
                {"user" => {"username" => "pedant_user"}},
                {"user" => {"username" => username}}
              ]})
        end
      end
    end # context DELETE /organizations/<org>/users/<name>
  end # context /organizations/<org>/users/<name>

  context "/users endpoint" do
    let(:request_url) { "#{platform.server}/users" }

    context "GET /users" do
      let(:users_body) do
        {
          # There are other users, but these are ours, so they should always be
          # somewhere in the userspace soup.
          "pivotal" => "#{request_url}/pivotal",
          "pedant-nobody" => "#{request_url}/pedant-nobody",
          "pedant_admin_user" => "#{request_url}/pedant_admin_user",
          "pedant_user" => "#{request_url}/pedant_user"
        }
      end

      context "superuser" do
        it "can get all users", :smoke do
          get(request_url, platform.superuser).should look_like({
              :status => 200,
              :body => users_body
            })
        end
      end

      context "admin user" do
        it "returns 403", :smoke do
          get(request_url, platform.admin_user).should look_like({
              :status => 403
            })
        end
      end

      context "default normal user" do
        it "returns 403" do
          get(request_url, platform.non_admin_user).should look_like({
              :status => 403
            })
        end
      end

      context "default client" do
        it "returns 401" do
          get(request_url, platform.non_admin_client).should look_like({
              :status => 401
            })
        end
      end

      context "outside user" do
        it "returns 403" do
          get(request_url, outside_user).should look_like({
              :status => 403
            })
        end
      end

      context "invalid user" do
        it "returns 401" do
          get(request_url, invalid_user).should look_like({
              :status => 401
            })
        end
      end
    end # context GET /users

    context "PUT /users" do
      context "admin user" do
        # A 405 here would be fine (and is no doubt coming with erlang)
        it "returns 404 (or 405?)" do
          put(request_url, platform.admin_user).should look_like({
              :status => ruby? ? 404 : 405
            })
        end
      end
    end # context PUT /users

    context "POST /users" do
      let(:username) { "test-#{Time.now.to_i}-#{Process.pid}" }
      let(:user_url) { "#{request_url}/#{username}" }
      let(:request_body) do
        {
          "username" => username,
          "email" => "#{username}@opscode.com",
          "first_name" => username,
          "last_name" => username,
          "display_name" => username,
          "password" => "badger badger"
        }
      end

      let(:response_body) do
        {
          "uri" => "#{platform.server}/users/#{username}",
          "private_key" => /^-----BEGIN RSA PRIVATE KEY-----/
        }
      end

      let(:users_with_new_user) do
        {
          # There are other users, but these are ours, so they should always be
          # somewhere in the userspace soup:
          "pivotal" => "#{request_url}/pivotal",
          "pedant-nobody" => "#{request_url}/pedant-nobody",
          "pedant_admin_user" => "#{request_url}/pedant_admin_user",
          "pedant_user" => "#{request_url}/pedant_user",
          # As should our test user:
          username => user_url
        }
      end

      after :each do
        # For the test with a space: we can't create it -- but also can't delete it,
        # ne?  No naked spaces allowed in URLs.
        if (username !~ / /)
          delete("#{platform.server}/users/#{username}", platform.superuser)
        end
      end

      context "superuser" do
        it "can create new user", :smoke do
          post(request_url, platform.superuser,
            :payload => request_body).should look_like({
              :status => 201,
              :body_exact => response_body
            })
          get(request_url, platform.superuser).should look_like({
              :status => 200,
              :body => users_with_new_user
            })
          get(user_url, platform.superuser).should look_like({
              :status => 200
            })
        end
      end

      context "admin user" do
        it "returns 403", :smoke do
          post(request_url, platform.admin_user,
            :payload => request_body).should look_like({
              :status => 403
            })
          get(user_url, platform.superuser).should look_like({
              :status => 404
            })
        end
      end

      context "creating users" do
        context "without password" do
          let(:request_body) do
            {
              "username" => username,
              "email" => "#{username}@opscode.com",
              "first_name" => username,
              "last_name" => username,
              "display_name" => username
            }
          end

          it "returns 400" do
            pending "actually returns 500" do
              post(request_url, platform.superuser,
                :payload => request_body).should look_like({
                  :status => 400
                })
            end
          end
        end

        context "without display_name" do
          let(:request_body) do
            {
              "username" => username,
              "email" => "#{username}@opscode.com",
              "first_name" => username,
              "last_name" => username,
              "password" => "badger badger"
            }
          end

          it "returns 400" do
            pending "actually returns 500" do
              post(request_url, platform.superuser,
                :payload => request_body).should look_like({
                  :status => 400
                })
            end
          end
        end

        context "without first and last name" do
          let(:request_body) do
            {
              "username" => username,
              "email" => "#{username}@opscode.com",
              "display_name" => username,
              "password" => "badger badger"
            }
          end

          it "can create new user" do
            post(request_url, platform.superuser,
              :payload => request_body).should look_like({
                :status => 201,
                :body_exact => response_body
              })
            get(request_url, platform.superuser).should look_like({
                :status => 200,
                :body => users_with_new_user
              })
          end
        end

        context "without email" do
          let(:request_body) do
            {
              "username" => username,
              "first_name" => username,
              "last_name" => username,
              "display_name" => username,
              "password" => "badger badger"
            }
          end

          it "returns 400" do
            pending "actually returns 500" do
              post(request_url, platform.superuser,
                :payload => request_body).should look_like({
                  :status => 400
                })
            end
          end
        end

        context "without username" do
          let(:request_body) do
            {
              "email" => "#{username}@opscode.com",
              "first_name" => username,
              "last_name" => username,
              "display_name" => username,
              "password" => "badger badger"
            }
          end

          it "returns 400" do
            pending "actually returns 500" do
              post(request_url, platform.superuser,
                :payload => request_body).should look_like({
                  :status => 400
                })
            end
          end
        end

        context "with invalid email" do
          let(:request_body) do
            {
              "username" => username,
              "email" => "#{username}@foo @ bar ahhh it's eating my eyes",
              "first_name" => username,
              "last_name" => username,
              "display_name" => username,
              "password" => "badger badger"
            }
          end

          it "returns 400" do
            pending "actually returns 500" do
              post(request_url, platform.superuser,
                :payload => request_body).should look_like({
                  :status => 400
                })
            end
          end
        end

        context "with spaces in names" do
          let(:request_body) do
            {
              "username" => username,
              "email" => "#{username}@opscode.com",
              "first_name" => "Yi Ling",
              "last_name" => "van Dijk",
              "display_name" => username,
              "password" => "badger badger"
            }
          end

          it "can create new user" do
            post(request_url, platform.superuser,
              :payload => request_body).should look_like({
                :status => 201,
                :body_exact => response_body
              })
            get(request_url, platform.superuser).should look_like({
                :status => 200,
                :body => users_with_new_user
              })
          end
        end

        context "with bogus field" do
          let(:request_body) do
            {
              "username" => username,
              "email" => "#{username}@opscode.com",
              "first_name" => username,
              "last_name" => username,
              "display_name" => username,
              "password" => "badger badger",
              "bogus" => "look at me"
            }
          end

          it "can create new user" do
            post(request_url, platform.superuser,
              :payload => request_body).should look_like({
                :status => 201,
                :body_exact => response_body
              })
            get(request_url, platform.superuser).should look_like({
                :status => 200,
                :body => users_with_new_user
              })
          end
        end

        context "with space in display_name" do
          let(:request_body) do
            {
              "username" => username,
              "email" => "#{username}@opscode.com",
              "first_name" => username,
              "last_name" => username,
              "display_name" => "some user",
              "password" => "badger badger"
            }
          end

          it "can create new user" do
            post(request_url, platform.superuser,
              :payload => request_body).should look_like({
                :status => 201,
                :body_exact => response_body
              })
            get(request_url, platform.superuser).should look_like({
                :status => 200,
                :body => users_with_new_user
              })
          end
        end

        context "with UTF-8 in display_name" do
          let(:request_body) do
            {
              "username" => username,
              "email" => "#{username}@opscode.com",
              "first_name" => username,
              "last_name" => username,
              "display_name" => "超人",
              "password" => "badger badger"
            }
          end

          it "can create new user" do
            post(request_url, platform.superuser,
              :payload => request_body).should look_like({
                :status => 201,
                :body_exact => response_body
              })
            get(request_url, platform.superuser).should look_like({
                :status => 200,
                :body => users_with_new_user
              })
          end
        end

        context "with UTF-8 in first/last name" do
          let(:request_body) do
            {
              "username" => username,
              "email" => "#{username}@opscode.com",
              "first_name" => "Guðrún",
              "last_name" => "Guðmundsdóttir",
              "display_name" => username,
              "password" => "badger badger"
            }
          end

          it "can create new user" do
            post(request_url, platform.superuser,
              :payload => request_body).should look_like({
                :status => 201,
                :body_exact => response_body
              })
            get(request_url, platform.superuser).should look_like({
                :status => 200,
                :body => users_with_new_user
              })
          end
        end

        context "with capitalized username" do
          let(:username) { "Test-#{Time.now.to_i}-#{Process.pid}" }
          let(:request_body) do
            {
              "username" => username,
              "email" => "#{username}@opscode.com",
              "first_name" => username,
              "last_name" => username,
              "display_name" => username,
              "password" => "badger badger"
            }
          end

          it "returns 400" do
            pending "actually returns 500" do
              post(request_url, platform.superuser,
                :payload => request_body).should look_like({
                  :status => 400
                })
            end
          end
        end

        context "with space in username" do
          let(:username) { "test #{Time.now.to_i}-#{Process.pid}" }
          let(:request_body) do
            {
              "username" => username,
              "email" => "#{username}@opscode.com",
              "first_name" => username,
              "last_name" => username,
              "display_name" => username,
              "password" => "badger badger"
            }
          end

          it "returns 400" do
            pending "actually returns 500" do
              post(request_url, platform.superuser,
                :payload => request_body).should look_like({
                  :status => 400
                })
            end
          end
        end

        context "when user already exists" do
          let(:request_body) do
            {
              "username" => username,
              "email" => "#{username}@opscode.com",
              "first_name" => username,
              "last_name" => username,
              "display_name" => username,
              "password" => "badger badger"
            }
          end

          it "returns 409" do
            post(request_url, platform.superuser,
              :payload => request_body).should look_like({
                :status => 201,
                :body_exact => response_body
              })
            get(request_url, platform.superuser).should look_like({
                :status => 200,
                :body => users_with_new_user
              })
            post(request_url, platform.superuser,
              :payload => request_body).should look_like({
                :status => 409
              })
          end
        end
      end # context creating users
    end # context POST /users

    context "DELETE /users" do
      context "admin user" do
        # A 405 here would be fine (and is no doubt coming with erlang)
        it "returns 404 (or 405?)" do
          delete(request_url, platform.admin_user).should look_like({
              :status => ruby? ? 404 : 405
            })
        end
      end
    end # context DELETE /users
  end # context /users endpoint

  context "/users/<name> endpoint" do
    let(:username) { platform.non_admin_user.name }
    let(:request_url) { "#{platform.server}/users/#{username}" }

    context "GET /users/<name>" do
      let(:user_body) do
        {
          "first_name" => username,
          "last_name" => username,
          "display_name" => username,
          "email" => "#{username}@opscode.com",
          "username" => username,
          "public_key" => /^-----BEGIN PUBLIC KEY-----/
        }
      end

      context "superuser" do
        it "can get user" do
          get(request_url, platform.superuser).should look_like({
              :status => 200,
              :body_exact => user_body
            })
        end
      end

      context "admin user" do
        it "can get user" do
          get(request_url, platform.admin_user).should look_like({
              :status => 200,
              :body_exact => user_body
            })
        end
      end

      context "default normal user" do
        it "can get self" do
          get(request_url, platform.non_admin_user).should look_like({
              :status => 200,
              :body_exact => user_body
            })
        end
      end

      context "default client" do
        it "returns 401" do
          get(request_url, platform.non_admin_client).should look_like({
              :status => 401
            })
        end
      end

      context "outside user" do
        it "returns 403" do
          get(request_url, outside_user).should look_like({
              :status => 403
            })
        end
      end

      context "invalid user" do
        it "returns 401" do
          get(request_url, invalid_user).should look_like({
              :status => 401
            })
        end
      end

      context "when user doesn't exist" do
        let(:username) { "bogus" }
        it "returns 404" do
          get(request_url, platform.superuser).should look_like({
              :status => 404
            })
        end
      end
    end # context GET /users/<name>

    context "PUT /users/<name>" do
      let(:username) { "test-#{Time.now.to_i}-#{Process.pid}" }
      let(:request_body) do
        {
          "username" => username,
          "email" => "#{username}@opscode.com",
          "first_name" => username,
          "last_name" => username,
          "display_name" => "new name",
          "password" => "badger badger"
        }
      end

      let(:response_body) do
        {
          "uri" => "#{platform.server}/users/#{username}"
        }
      end

      let(:modified_user) do
        {
          "username" => username,
          "email" => "#{username}@opscode.com",
          "first_name" => username,
          "last_name" => username,
          "display_name" => "new name",
          "public_key" => /^-----BEGIN PUBLIC KEY----/
        }
      end

      before :each do
        post("#{platform.server}/users", platform.superuser,
          :payload => {
            "username" => username,
            "email" => "#{username}@opscode.com",
            "first_name" => username,
            "last_name" => username,
            "display_name" => username,
            "password" => "badger badger"
          }).should look_like({
            :status => 201,
            :body_exact => {
              "uri" => "#{platform.server}/users/#{username}",
              "private_key" => /^-----BEGIN RSA PRIVATE KEY-----/
            }})
      end

      after :each do
        delete("#{platform.server}/users/#{username}", platform.superuser)
      end

      context "superuser" do
        it "can modify user", :smoke do
          put(request_url, platform.superuser,
            :payload => request_body).should look_like({
              :status => 200,
              :body_exact => response_body
            })
          get(request_url, platform.superuser).should look_like({
              :status => 200,
              :body_exact => modified_user
            })
        end
      end

      context "admin user" do
        it "returns 403", :smoke do
          put(request_url, platform.admin_user,
            :payload => request_body).should look_like({
              :status => 403
            })
        end
      end

      context "default client" do
        it "returns 401" do
          put(request_url, platform.non_admin_client,
            :payload => request_body).should look_like({
              :status => 401
            })
        end
      end

      context "when modifying non-existent user" do
        let(:request_url) { "#{platform.server}/users/bogus" }
        it "returns 404" do
          put(request_url, platform.superuser,
            :payload => request_body).should look_like({
              :status => 404
            })
        end
      end

      context "modifying users" do
        context "without password" do
          let(:request_body) do
            {
              "username" => username,
              "email" => "#{username}@opscode.com",
              "first_name" => username,
              "last_name" => username,
              "display_name" => "new name"
            }
          end

          it "can modify user" do
            put(request_url, platform.superuser,
              :payload => request_body).should look_like({
                :status => 200
              })
            get(request_url, platform.superuser).should look_like({
                :status => 200,
                :body_exact => modified_user
              })
          end
        end

        context "with bogus field" do
          let(:request_body) do
            {
              "username" => username,
              "email" => "#{username}@opscode.com",
              "first_name" => username,
              "last_name" => username,
              "display_name" => "new name",
              "password" => "badger badger",
              "bogus" => "not a badger"
            }
          end

          it "can modify user" do
            put(request_url, platform.superuser,
              :payload => request_body).should look_like({
                :status => 200
              })
            get(request_url, platform.superuser).should look_like({
                :status => 200,
                :body_exact => modified_user
              })
          end
        end

        context "without display_name" do
          let(:request_body) do
            {
              "username" => username,
              "email" => "#{username}@opscode.com",
              "first_name" => username,
              "last_name" => username,
              "password" => "badger badger"
            }
          end

          it "returns 400" do
            pending "actually returns 500" do
              put(request_url, platform.superuser,
                :payload => request_body).should look_like({
                  :status => 400
                })
            end
          end
        end

        context "without first and last name" do
          let(:request_body) do
            {
              "username" => username,
              "email" => "#{username}@opscode.com",
              "display_name" => "new name",
              "password" => "badger badger"
            }
          end

          let(:modified_user) do
            {
              "username" => username,
              "email" => "#{username}@opscode.com",
              "display_name" => "new name",
              "public_key" => /^-----BEGIN PUBLIC KEY----/
            }
          end

          it "can modify user" do
            put(request_url, platform.superuser,
              :payload => request_body).should look_like({
                :status => 200
              })
            get(request_url, platform.superuser).should look_like({
                :status => 200,
                :body => modified_user
              })
          end
        end

        context "without email" do
          let(:request_body) do
            {
              "username" => username,
              "first_name" => username,
              "last_name" => username,
              "display_name" => username,
              "password" => "badger badger"
            }
          end

          it "returns 400" do
            pending "actually returns 500" do
              put(request_url, platform.superuser,
                :payload => request_body).should look_like({
                  :status => 400
                })
            end
          end
        end

        context "without username" do
          let(:request_body) do
            {
              "email" => "#{username}@opscode.com",
              "first_name" => username,
              "last_name" => username,
              "display_name" => username,
              "password" => "badger badger"
            }
          end

          it "returns 400" do
            pending "actually returns 500" do
              put(request_url, platform.superuser,
                :payload => request_body).should look_like({
                  :status => 400
                })
            end
          end
        end

        context "with invalid email" do
          let(:request_body) do
            {
              "username" => username,
              "email" => "#{username}@foo @ bar no go",
              "first_name" => username,
              "last_name" => username,
              "display_name" => username,
              "password" => "badger badger"
            }
          end

          it "returns 400" do
            pending "actually returns 500" do
              put(request_url, platform.superuser,
                :payload => request_body).should look_like({
                  :status => 400
                })
            end
          end
        end

        context "with spaces in names" do
          let(:request_body) do
            {
              "username" => username,
              "email" => "#{username}@opscode.com",
              "first_name" => "Ren Kai",
              "last_name" => "de Boers",
              "display_name" => username,
              "password" => "badger badger"
            }
          end

          let(:modified_user) do
            {
              "username" => username,
              "email" => "#{username}@opscode.com",
              "first_name" => "Ren Kai",
              "last_name" => "de Boers",
              "display_name" => username,
              "public_key" => /^-----BEGIN PUBLIC KEY----/
            }
          end

          it "can modify user" do
            put(request_url, platform.superuser,
              :payload => request_body).should look_like({
                :status => 200
              })
            get(request_url, platform.superuser).should look_like({
                :status => 200,
                :body => modified_user
              })
          end
        end

        context "with space in display_name" do
          let(:request_body) do
            {
              "username" => username,
              "email" => "#{username}@opscode.com",
              "first_name" => username,
              "last_name" => username,
              "display_name" => "some user",
              "password" => "badger badger"
            }
          end

          let(:modified_user) do
            {
              "username" => username,
              "email" => "#{username}@opscode.com",
              "first_name" => username,
              "last_name" => username,
              "display_name" => "some user",
              "public_key" => /^-----BEGIN PUBLIC KEY----/
            }
          end

          it "can modify user" do
            put(request_url, platform.superuser,
              :payload => request_body).should look_like({
                :status => 200
              })
            get(request_url, platform.superuser).should look_like({
                :status => 200,
                :body => modified_user
              })
          end
        end

        context "with UTF-8 in display_name" do
          let(:request_body) do
            {
              "username" => username,
              "email" => "#{username}@opscode.com",
              "first_name" => username,
              "last_name" => username,
              "display_name" => "ギリギリ",
              "password" => "badger badger"
            }
          end

          let(:modified_user) do
            {
              "username" => username,
              "email" => "#{username}@opscode.com",
              "first_name" => username,
              "last_name" => username,
              "display_name" => "ギリギリ",
              "public_key" => /^-----BEGIN PUBLIC KEY----/
            }
          end

          it "can modify user" do
            put(request_url, platform.superuser,
              :payload => request_body).should look_like({
                :status => 200
              })
            get(request_url, platform.superuser).should look_like({
                :status => 200,
                :body => modified_user
              })
          end
        end

        context "with UTF-8 in first/last name" do
          let(:request_body) do
            {
              "username" => username,
              "email" => "#{username}@opscode.com",
              "first_name" => "Eliška",
              "last_name" => "Horáčková",
              "display_name" => username,
              "password" => "badger badger"
            }
          end

          let(:modified_user) do
            {
              "username" => username,
              "email" => "#{username}@opscode.com",
              "first_name" => "Eliška",
              "last_name" => "Horáčková",
              "display_name" => username,
              "public_key" => /^-----BEGIN PUBLIC KEY----/
            }
          end

          it "can modify user" do
            put(request_url, platform.superuser,
              :payload => request_body).should look_like({
                :status => 200
              })
            get(request_url, platform.superuser).should look_like({
                :status => 200,
                :body => modified_user
              })
          end
        end
      end # context modifying users

      context "renaming users" do
        let(:new_name) { "test2-#{Time.now.to_i}-#{Process.pid}" }
        let(:new_request_url) { "#{platform.server}/users/#{new_name}" }

        context "changing username" do
          let(:request_body) do
            {
              "username" => new_name,
              "email" => "#{username}@opscode.com",
              "first_name" => username,
              "last_name" => username,
              "display_name" => username,
              "password" => "badger badger"
            }
          end

          let(:modified_user) do
            {
              "username" => new_name,
              "email" => "#{username}@opscode.com",
              "first_name" => username,
              "last_name" => username,
              "display_name" => username,
              "public_key" => /^-----BEGIN PUBLIC KEY----/
            }
          end

          after :each do
            delete("#{platform.server}/users/#{new_name}", platform.superuser)
          end

          it "renames user" do
            put(request_url, platform.superuser,
              :payload => request_body).should look_like({
                :status => 201
              })
            get(request_url, platform.superuser).should look_like({
                :status => 404
              })
            get(new_request_url, platform.superuser).should look_like({
                :status => 200,
                :body_exact => modified_user
              })
          end
        end

        context "changing username with UTF-8" do
          let(:new_name) { "テスト-#{Time.now.to_i}-#{Process.pid}" }

          let(:request_body) do
            {
              "username" => new_name,
              "email" => "#{username}@opscode.com",
              "first_name" => username,
              "last_name" => username,
              "display_name" => username,
              "password" => "badger badger"
            }
          end

          it "returns 400" do
            pending "actually returns 500" do
              put(request_url, platform.superuser,
                :payload => request_body).should look_like({
                  :status => 400
                })
              get(request_url, platform.superuser).should look_like({
                  :status => 200
                })
              get(new_request_url, platform.superuser).should look_like({
                  :status => 404
                })
            end
          end
        end

        context "changing username with spaces" do
          let(:new_name) { "test #{Time.now.to_i}-#{Process.pid}" }

          let(:request_body) do
            {
              "username" => new_name,
              "email" => "#{username}@opscode.com",
              "first_name" => username,
              "last_name" => username,
              "display_name" => username,
              "password" => "badger badger"
            }
          end

          it "returns 400" do
            pending "actually returns 500" do
              put(request_url, platform.superuser,
                :payload => request_body).should look_like({
                  :status => 400
                })
              get(request_url, platform.superuser).should look_like({
                  :status => 200
                })
              get(new_request_url, platform.superuser).should look_like({
                  :status => 404
                })
            end
          end
        end

        context "changing username with capital letters" do
          let(:new_name) { "Test-#{Time.now.to_i}-#{Process.pid}" }

          let(:request_body) do
            {
              "username" => new_name,
              "email" => "#{username}@opscode.com",
              "first_name" => username,
              "last_name" => username,
              "display_name" => username,
              "password" => "badger badger"
            }
          end

          it "returns 400" do
            pending "actually returns 500" do
              put(request_url, platform.superuser,
                :payload => request_body).should look_like({
                  :status => 400
                })
              get(request_url, platform.superuser).should look_like({
                  :status => 200
                })
              get(new_request_url, platform.superuser).should look_like({
                  :status => 404
                })
            end
          end
        end

        context "new name already exists" do
          let(:request_body) do
            {
              "username" => platform.non_admin_user.name,
              "email" => "#{username}@opscode.com",
              "first_name" => username,
              "last_name" => username,
              "display_name" => username,
              "password" => "badger badger"
            }
          end

          let(:unmodified_user) do
            {
              "username" => username,
              "email" => "#{username}@opscode.com",
              "first_name" => username,
              "last_name" => username,
              "display_name" => username,
              "public_key" => /^-----BEGIN PUBLIC KEY----/
            }
          end

          it "returns 409" do
            pending "actually returns 500" do
              put(request_url, platform.superuser,
                :payload => request_body).should look_like({
                  :status => 409
                })
              get(request_url, platform.superuser).should look_like({
                  :status => 200,
                  :body_exact => unmodified_user
                })
            end
          end
        end
      end # context renaming users
    end # context PUT /users/<name>

    context "POST /users/<name>" do
      context "admin user" do
        # A 405 here would be fine (and is no doubt coming with erlang)
        it "returns 404 (or 405?)" do
          post(request_url, platform.admin_user).should look_like({
              :status => ruby? ? 404 : 405
            })
        end
      end
    end # context POST /users/<name>

    context "DELETE /users/<name>" do
      let(:username) { "test-#{Time.now.to_i}-#{Process.pid}" }

      before :each do
        post("#{platform.server}/users", platform.superuser,
          :payload => {
            "username" => username,
            "email" => "#{username}@opscode.com",
            "first_name" => username,
            "last_name" => username,
            "display_name" => username,
            "password" => "badger badger"
          }).should look_like({
            :status => 201,
            :body_exact => {
              "uri" => "#{platform.server}/users/#{username}",
              "private_key" => /^-----BEGIN RSA PRIVATE KEY-----/
            }})
      end

      after :each do
        delete("#{platform.server}/users/#{username}", platform.superuser)
      end

      context "superuser" do
        it "can delete user" do
          delete(request_url, platform.superuser).should look_like({
              :status => 200
            })
          get("#{platform.server}/users/#{username}",
            platform.admin_user).should look_like({
              :status => 404
            })
        end
      end

      context "admin user" do
        it "returns 403" do
          delete(request_url, platform.admin_user).should look_like({
              :status => 403
            })
          get("#{platform.server}/users/#{username}",
            platform.superuser).should look_like({
              :status => 200
            })
        end
      end

      context "default client" do
        it "returns 401" do
          delete(request_url, platform.non_admin_client).should look_like({
              :status => 401
            })
          get("#{platform.server}/users/#{username}",
            platform.superuser).should look_like({
              :status => 200
            })
        end
      end

      context "when deleting a non-existent user" do
          let(:request_url) { "#{platform.server}/users/bogus" }
        it "returns 404" do
          delete(request_url, platform.superuser).should look_like({
              :status => 404
            })
        end
      end
    end # context DELETE /users/<name>
  end # context /users/<name> endpoint
end # describe users
