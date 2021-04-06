# -*- coding: utf-8 -*-
require 'pedant/rspec/common'

describe "users", :users do

  let(:public_key_regex) do
    /^-----BEGIN (RSA )?PUBLIC KEY-----/
  end

  let(:private_key_regex) do
    /^-----BEGIN (RSA )?PRIVATE KEY-----/
  end

  # Pedant has configurable test users.
  # Selects Pedant users that are marked as associated
  let(:default_pedant_user_names) { platform.users.select(&:associate).map(&:name).sort }
  let(:default_users_body)        { default_pedant_user_names.map { |user| {"user" => {"username" => user} } } }

  context "/users endpoint" do
    let(:request_url) { "#{platform.server}/users" }

    context "GET /users" do
      let(:users_body) do
        {
          # There are other users, but these are ours, so they should always be
          # somewhere in the userspace soup.
          "pivotal" => "#{request_url}/pivotal",
          platform.bad_user.name => "#{request_url}/#{platform.bad_user.name}",
          platform.admin_user.name => "#{request_url}/#{platform.admin_user.name}",
          platform.non_admin_user.name => "#{request_url}/#{platform.non_admin_user.name}",
        }
      end
      let(:empty_users_body) do
        {}
      end
      let(:filtered_users_body) do
        {
          platform.non_admin_user.name => "#{request_url}/#{platform.non_admin_user.name}"
        }
      end

      context "superuser" do
        it "can get all users", :smoke do
          get(request_url, platform.superuser).should look_like({
              :status => 200,
              :body => users_body
            })
        end

        it "returns no users when filtering by non-existing email", :smoke do
          get("#{request_url}?email=somenonexistingemail@somewhere.com", platform.superuser).should look_like({
              :status => 200,
              :body_exact => empty_users_body
            })
        end

        it "returns a single user when filtering by that user's email address", :smoke do
          # Let's get a known user and mail address.
          response = get("#{request_url}/#{platform.non_admin_user.name}", platform.superuser)
          email = JSON.parse(response)["email"]
          get("#{request_url}?email=#{email}", platform.superuser).should look_like({
              :status => 200,
              :body_exact => filtered_users_body
            })
        end

        context "saml user" do
          let(:username) do
            "user_with_external_uid"
          end

          let(:external_auth_id) do
            "sam.l.jackson"
          end

          let(:user_options) do
            { :overrides => {
                "first_name" => "external",
                "last_name" => "user",
                "display_name" => "SAML USER",
                "external_authentication_uid" => external_auth_id
              }
            }
          end

          let(:filtered_external_users_body) do
            { username => "#{request_url}/#{username}" }
          end

          before :each do
            @user = platform.create_user(username, user_options)
          end

          after :each do
            platform.delete_user(@user)
          end

          it "returns no users when filtering by non-existing external_authentication_uid", :smoke do
            get("#{request_url}?external_authentication_uid=somenonexistingemail@somewhere.com", platform.superuser).should look_like({
                :status => 200,
                :body_exact => empty_users_body
              })
          end

          it "returns a single user when filtering by that user's external_authentication_uid", :smoke do
            get("#{request_url}?external_authentication_uid=#{external_auth_id}", platform.superuser).should look_like({
                :status => 200,
                :body_exact => filtered_external_users_body
              })
          end
        end

        it "returns a verbose list of users upon request" do
          body = JSON.parse(get("#{request_url}?verbose=true", platform.superuser))
          [ platform.non_admin_user.name, platform.admin_user.name, platform.superuser.name ].each do |name|
            data = body[name]
            data.should_not be nil
            data.key?("display_name").should be true
            data.key?("first_name").should be true
            data.key?("last_name").should be true
            data.key?("email").should be true
          end
        end

        context "case-insensitive email addresses" do
          let(:username) { "user_for_email_tests" }
          let(:email) { "User@aol.com" }
          let(:user_options) do
            { :overrides => {
                "first_name" => "user",
                "last_name" => "user",
                "display_name" => "USER USER",
                "email" => email
              }
            }
          end

          let(:filtered_users_body) do
            { username => "#{request_url}/#{username}" }
          end

          before :each do
            @user = platform.create_user(username, user_options)
          end

          after :each do
            platform.delete_user(@user)
          end

          it "finds a user by email if the query does matches what is stored" do
            get("#{request_url}?email=#{email}", platform.superuser).should look_like({
                :status => 200,
                :body_exact => filtered_users_body
              })
          end

          it "finds a user by email if the query is all uppercase" do
            uppercase = 'USER@AOL.COM'
            get("#{request_url}?email=#{uppercase}", platform.superuser).should look_like({
                :status => 200,
                :body_exact => filtered_users_body
              })
          end

          it "finds a user by email if the query is all lowercase" do
            lowercase = 'user@aol.com'
            get("#{request_url}?email=#{lowercase}", platform.superuser).should look_like({
                :status => 200,
                :body_exact => filtered_users_body
              })
          end

          context "when there's another user with that mail (different case)", :email_case_insensitive do
            let(:username_two) { "user_for_email_tests_two" }
            let(:email_two) { "useR@aOl.com" }
            let(:user_options_two) do
              { :overrides => {
                  "first_name" => "user",
                  "last_name" => "user",
                  "display_name" => "USER USER",
                  "email" => email_two
                }
              }
            end
            let(:user_two) do
              {
                "username" => username_two,
                "email" => email_two,
                "first_name" => username,
                "last_name" => username,
                "display_name" => username,
                "password" => "foobar"
              }
            end

            let(:filtered_users_body) do
              { username => "#{request_url}/#{username}",
              }
            end

            before :each do
              begin
                platform.create_user(username_two, user_options_two)
              rescue StandardError => e  
                puts e
                puts "Username or email address already in use."
              end
            end

            it "finds only one users by this email" do
              get("#{request_url}?email=#{email}", platform.superuser).should look_like({
                  :status => 200,
                  :body_exact => filtered_users_body
              })
            end

            it "conflict when trying to create a user with duplicate case-insensitive email address." do
              post(request_url, platform.superuser, :payload => user_two).should look_like({
                  :status => 409,
                  :body_exact => {"error"=>["Username or email address already in use."]}
              })
            end
          end
        end

      end

      context "admin user" do
        it "returns 403", :authorization, :smoke do
          get(request_url, platform.admin_user).should look_like({
              :status => 403
            })
        end
      end

      context "default normal user" do
        it "returns 403", :authorization do
          get(request_url, platform.non_admin_user).should look_like({
              :status => 403
            })
        end
      end

      context "default client" do
        it "returns 401", :authentication do
          get(request_url, platform.non_admin_client).should look_like({
              :status => 401
            })
        end
      end

      context "outside user" do
        it "returns 403", :authorization do
          get(request_url, outside_user).should look_like({
              :status => 403
            })
        end
      end

      context "invalid user" do
        it "returns 401", :authentication do
          get(request_url, invalid_user).should look_like({
              :status => 401
            })
        end
      end
    end # context GET /users

    context "PUT /users" do
      context "admin user" do
        it "returns  405" do
          put(request_url, platform.admin_user).should look_like({
              :status => 405
            })
        end
      end
    end # context PUT /users

    context "POST /users" do
      let(:username) { "test-#{Process.pid}" }
      let(:user_url) { "#{request_url}/#{username}" }
      let(:request_body) do
        {
          "username" => username,
          "email" => "#{username}@chef.io",
          "first_name" => username,
          "last_name" => username,
          "display_name" => username,
          "password" => "badger badger"
        }
      end

      let(:response_body) do
        {
          "uri" => "#{platform.server}/users/#{username}",
          "private_key" => private_key_regex
        }
      end

      let(:users_with_new_user) do
        {
          # There are other users, but these are ours, so they should always be
          # somewhere in the userspace soup:
          "pivotal" => "#{request_url}/pivotal",
          platform.bad_user.name => "#{request_url}/#{platform.bad_user.name}",
          platform.admin_user.name => "#{request_url}/#{platform.admin_user.name}",
          platform.non_admin_user.name => "#{request_url}/#{platform.non_admin_user.name}",
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
        it "returns 403", :authorization, :smoke do
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
              "email" => "#{username}@chef.io",
              "first_name" => username,
              "last_name" => username,
              "display_name" => username
            }
          end

          it "returns 400", :validation do
            post(request_url, platform.superuser,
                 :payload => request_body).should look_like({
                   :status => 400
                 })
          end
        end
        context "with external auth enabled" do
          let(:request_body) do
            {
              "username" => username,
              "email" => "#{username}@chef.io",
              "first_name" => username,
              "last_name" => username,
              "display_name" => username,
              "external_authentication_uid" => username
            }
          end

          it "returns 201 when password is not provided" do
            post(request_url, platform.superuser, :payload => request_body).should look_like({
                   :status => 201
                 })
          end
          it "returns 201 when password is provided" do
            final_body = request_body.merge( { "password" => "foo bar"} )
            post(request_url, platform.superuser, :payload => final_body).should look_like({
                   :status => 201
                 })
          end
        end

        context "without display_name" do
          let(:request_body) do
            {
              "username" => username,
              "email" => "#{username}@chef.io",
              "first_name" => username,
              "last_name" => username,
              "password" => "badger badger"
            }
          end

          it "returns 400", :validation do
            post(request_url, platform.superuser,
                 :payload => request_body).should look_like({
                   :status => 400
                 })
          end
        end

        context "without first and last name" do
          let(:request_body) do
            {
              "username" => username,
              "email" => "#{username}@chef.io",
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

        context "with a blank middle name" do
          let(:request_body) do
            {
              "username" => username,
              "email" => "#{username}@chef.io",
              "display_name" => username,
              "password" => "badger badger",
              "first_name" => username,
              "last_name" => username,
              "middle_name" => "",
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

        context "with a blank first name" do
          let(:request_body) do
            {
              "username" => username,
              "email" => "#{username}@chef.io",
              "display_name" => username,
              "password" => "badger badger",
              "first_name" => "",
              "last_name" => username,
              "middle_name" => username,
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

        context "with a blank last name" do
          let(:request_body) do
            {
              "username" => username,
              "email" => "#{username}@chef.io",
              "display_name" => username,
              "password" => "badger badger",
              "first_name" => username,
              "last_name" => "",
              "middle_name" => username,
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

          it "returns 400", :validation do
            post(request_url, platform.superuser,
                 :payload => request_body).should look_like({
                   :status => 400
                 })
          end
        end
        context "without email but with external auth enabled" do
          let(:request_body) do
            {
              "username" => username,
              "first_name" => username,
              "last_name" => username,
              "display_name" => username,
              "password" => "badger badger",
              "external_authentication_uid" => username
            }
          end

          it "returns 201" do
            post(request_url, platform.superuser,
                 :payload => request_body).should look_like({
                   :status => 201
                 })
          end
        end
        context "without username" do
          let(:request_body) do
            {
              "email" => "#{username}@chef.io",
              "first_name" => username,
              "last_name" => username,
              "display_name" => username,
              "password" => "badger badger"
            }
          end

          it "returns 400", :validation do
            post(request_url, platform.superuser,
                 :payload => request_body).should look_like({
                   :status => 400
                 })
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

          it "returns 400", :validation do
            post(request_url, platform.superuser,
              :payload => request_body).should look_like({
                :status => 400
              })
          end
        end

        context "with spaces in names" do
          let(:request_body) do
            {
              "username" => username,
              "email" => "#{username}@chef.io",
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
              "email" => "#{username}@chef.io",
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
              "email" => "#{username}@chef.io",
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
              "email" => "#{username}@chef.io",
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
              "email" => "#{username}@chef.io",
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
          let(:username) { "Test-#{Process.pid}" }
          let(:request_body) do
            {
              "username" => username,
              "email" => "#{username}@chef.io",
              "first_name" => username,
              "last_name" => username,
              "display_name" => username,
              "password" => "badger badger"
            }
          end

          it "returns 400", :validation do
            post(request_url, platform.superuser,
                 :payload => request_body).should look_like({
                   :status => 400
                 })
          end
        end

        context "with space in username" do
          let(:username) { "test #{Time.now.to_i}-#{Process.pid}" }
          let(:request_body) do
            {
              "username" => username,
              "email" => "#{username}@chef.io",
              "first_name" => username,
              "last_name" => username,
              "display_name" => username,
              "password" => "badger badger"
            }
          end

          it "returns 400", :validation do
            post(request_url, platform.superuser,
                 :payload => request_body).should look_like({
                   :status => 400
                 })
          end
        end

        context "when user already exists" do
          let(:request_body) do
            {
              "username" => username,
              "email" => "#{username}@chef.io",
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
        it "returns  405" do
          delete(request_url, platform.admin_user).should look_like({
              :status => 405
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
          "email" => "#{username}@chef.io",
          "username" => username,
          "public_key" => public_key_regex
        }
      end

      context "create v0 user" do
        it "does not show the sentinel value to the user" do
          response = JSON.parse(get(request_url, platform.superuser))
          response["public_key"].should_not eql("this_in_not_a_key")
        end
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

        it "can get the admin user since they share an organization", :authorization do
          req_url = "#{platform.server}/users/#{platform.admin_user.name}"
          get(req_url, platform.non_admin_user).should look_like({
              :status => 200
           })
        end
      end

      context "default client" do
        it "returns 401", :authentication do
          get(request_url, platform.non_admin_client).should look_like({
              :status => 401
            })
        end
      end

      context "outside user" do
        it "returns 403", :authorization do
          get(request_url, outside_user).should look_like({
              :status => 403
            })
        end
      end

      context "invalid user" do
        it "returns 401", :authentication do
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

    context "PUT /users/<name> when user created w/ external auth enabled" do
      let(:username) { "test-#{Process.pid}" }
      let(:request_body) do
        {
          "username" => username,
          "email" => "#{username}@chef.io",
          "first_name" => username,
          "last_name" => username,
          "display_name" => "new name",
          "external_authentication_uid" => username
        }
      end
      before :each do
        response = post("#{platform.server}/users", platform.superuser,
                        :payload => {
                          "username" => username,
                          "first_name" => username,
                          "last_name" => username,
                          "display_name" => username,
                          "external_authentication_uid" => username
                        })
        response.should look_like({ :status => 201 })
      end

      after :each do
        delete("#{platform.server}/users/#{username}", platform.superuser)
      end


      context "without email and without specifying external auth uid" do
        let(:request_body) do
          {
            "username" => username,
            "display_name" => username
          }
        end

        it "returns 200" do
          put(request_url, platform.superuser,
              :payload => request_body).should look_like({
                                                           :status => 200
                                                         })
        end
      end
    end
    context "PUT /users/<name>" do
      let(:username) { "test-#{Process.pid}" }
      let(:password) { "badger badger" }

      let(:request_body) do
        {
          "username" => username,
          "email" => "#{username}@chef.io",
          "first_name" => username,
          "last_name" => username,
          "display_name" => "new name",
          "password" => password
        }
      end

      let(:request_body_with_ext_id) do
        {
          "username" => username,
          "email" => "#{username}@chef.io",
          "first_name" => username,
          "last_name" => username,
          "display_name" => "new name",
          "external_authentication_uid" => "bob"
        }
      end

      let(:request_body_with_recovery) do
        {
          "username" => username,
          "email" => "#{username}@chef.io",
          "first_name" => username,
          "last_name" => username,
          "display_name" => "new name",
          "recovery_authentication_enabled" => true
        }
      end

      let(:modified_user) do
        {
          "username" => username,
          "email" => "#{username}@chef.io",
          "first_name" => username,
          "last_name" => username,
          "display_name" => "new name",
          "public_key" => public_key_regex
        }
      end

      let(:input_public_key) do
        <<EOF
-----BEGIN PUBLIC KEY-----
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA+h5g/r/qaFH6OdYOG0OO
2/WpLb9qik7SPFmcOvujqZzLO2yv4kXwuvncx/ADHdkobaoFn3FE84uzIVCoSeaj
xTMeuTcPr5y+wsVqCYMkwIJpPezbwcrErt14BvD9BPN0UDyOJZW43ZN4iIw5xW8y
lQKuZtTNsm7FoznG+WsmRryTM3OjOrtDYjN/JHwDfrZZtVu7pT8FYnnz0O8j2zEf
9NALhpS7oDCf+VSo6UUk/w5m4/LpouDxT2dKBwQOuA8pzXd5jHP6rYdbHkroOUqx
Iy391UeSCiPVHcAN82sYV7R2MnUYj6b9Fev+62FKrQ6v9QYZcyljh6hldmcbmABy
EQIDAQAB
-----END PUBLIC KEY-----
EOF
      end

      let(:input_certificate) do
        <<EOF
-----BEGIN CERTIFICATE-----
MIIDNjCCAp+gAwIBAgIBATANBgkqhkiG9w0BAQUFADCBnjELMAkGA1UEBhMCVVM
EzARBgNVBAgMCldhc2hpbmd0b24xEDAOBgNVBAcMB1NlYXR0bGUxFjAUBgNVBAo
DU9wc2NvZGUsIEluYy4xHDAaBgNVBAsME0NlcnRpZmljYXRlIFNlcnZpY2UxMjA
BgNVBAMMKW9wc2NvZGUuY29tL2VtYWlsQWRkcmVzcz1hdXRoQG9wc2NvZGUuY29
GIBBERISHGIBBERISHGIBBERISHGIBBERISHGIBBERISHGIBBERISHGIBBERISH
44XkW6yWRQbShOQImeop9ODWTkZ4mR1p8+0YBIN9nJaVEQsq112w9SF84I23clE
YAl2yCI8v2GiDgeJbC4MheYITeBUpqquacSNYya7jWSEPuP3Hq4r0A9O7yZk+F9
cF4OxBcyrMVFaUaLLNzuYjom8Oe5rZIJqE2Je8ujOMMrWJQpCLrZNY6ZNivQQBf
tjl8Dbe7xvWWTtD/HdY0LL+UT5MUi/rUx0zfKPHgvsERM+r4sod6E6GhbMesP0i
+ewJb/FRGbyF6VDB5xYshHJ28MWpQ8KsW2An+tVwqRtaHWOGWXLXHaLkG3EOgv0
70sCAwEAATANBgkqhkiG9w0BAQUFAAOBgQAj+j4oxrg0Q6L3sEhDfxDy8dV+P/4
ik9R4ZgC1xooGZcEumJZgT/GhWBsCe97BZXntArgt1zKM2Ad30pElgbWoqbtGLX
B4ybSWTCCW3xnQw+bSDiSP/nZjo5RlxWk6bqLjA9+wTMsxotaMd4Sd5P64lZZrS
eXXVMgVGvcGheA==
-----END CERTIFICATE-----
EOF
      end

      before :each do
        response = post("#{platform.server}/users", platform.superuser,
          :payload => {
            "username" => username,
            "email" => "#{username}@chef.io",
            "first_name" => username,
            "last_name" => username,
            "display_name" => username,
            "password" => "badger badger"
          })

        # Verify required preconditions are in place
        response.should look_like({
            :status => 201,
            :body => { "private_key" => private_key_regex }})

        @original_private_key = JSON.parse(response.body)["private_key"]
      end

      after :each do
        delete("#{platform.server}/users/#{username}", platform.superuser)
        @original_private_key = nil
      end

      context "superuser" do
        it "can modify user", :smoke do
          put(request_url, platform.superuser,
            :payload => request_body).should look_like({
              :status => 200
            })
          get(request_url, platform.superuser).should look_like({
              :status => 200,
              :body_exact => modified_user
            })
        end

        context "authenticating after updates" do
          let(:auth_url) { "#{platform.server}/authenticate_user" }
          context "when password is unchanged" do
            it "can authenticate as the modified user when password has not been changed.", :smoke do
              put(request_url, platform.superuser, :payload => request_body).should look_like({ :status => 200 })
              post(auth_url, superuser, :payload => { 'username' => username,
                                                      'password' => password }).should look_like({
                  :status => 200
              })
            end
          end

          context "when password is updated" do
            let(:password) { "bidger bidger"}
            it "can authenticate as the modified user when password has been changed" do
              put(request_url, platform.superuser, :payload => request_body).should look_like({ :status => 200 })

              post(auth_url, superuser, :payload => { 'username' => username,
                                                      'password' => password }).should look_like({
                  :status => 200
              })

            end
            it "fails to authenticate as the modified user using the old password" do
              put(request_url, platform.superuser, :payload => request_body).should look_like({ :status => 200 })

              post(auth_url, superuser, :payload => { 'username' => username,
                                                      'password' => 'badger badger' }).should look_like({
                  :status => 401
              })
            end

          end
        end

        it "can enable recovery" do
          put(request_url, platform.superuser,
            :payload => request_body_with_recovery).should look_like({
              :status => 200
            })

        end
        it "can set external id" do
          put(request_url, platform.superuser,
            :payload => request_body_with_ext_id).should look_like({
              :status => 200
            })
        end
      end

      context "admin user" do
        it "returns 403", :authorization, :smoke do
          put(request_url, platform.admin_user,
            :payload => request_body).should look_like({
              :status => 403
            })
        end
        it "cannot enable recovery", :authorization do
          put(request_url, platform.admin_user,
            :payload => request_body_with_recovery).should look_like({
              :status => 403
            })

        end
        it "cannot set external id", :authorization do
          put(request_url, platform.admin_user,
            :payload => request_body_with_ext_id).should look_like({
              :status => 403
            })
        end
      end
      context "owning user" do

        it "can modify its own account", :authorization do
          put(request_url, platform.non_admin_user,
            :payload => request_body).should look_like({
              :status => 403
            })

        end
        it "cannot enable recovery", :authorization do
          put(request_url, platform.non_admin_user,
            :payload => request_body_with_recovery).should look_like({
              :status => 403
            })

        end
        it "cannot set external id", :authorization do
          put(request_url, platform.non_admin_user,
            :payload => request_body_with_ext_id).should look_like({
              :status => 403
            })
        end
      end

      context "default client" do
        it "returns 401", :authentication do
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
              "email" => "#{username}@chef.io",
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
              "email" => "#{username}@chef.io",
              "first_name" => username,
              "last_name" => username,
              "display_name" => "new name",
              "password" => "badger badger",
              "bogus" => "not a badger"
            }
          end

          it "can modify user", :validation do
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
              "email" => "#{username}@chef.io",
              "first_name" => username,
              "last_name" => username,
              "password" => "badger badger"
            }
          end

          it "returns 400", :validation do
            put(request_url, platform.superuser,
              :payload => request_body).should look_like({
                :status => 400
              })
          end
        end

        context "without first and last name" do
          let(:request_body) do
            {
              "username" => username,
              "email" => "#{username}@chef.io",
              "display_name" => "new name",
              "password" => "badger badger"
            }
          end

          let(:modified_user) do
            {
              "username" => username,
              "email" => "#{username}@chef.io",
              "display_name" => "new name",
              "public_key" => public_key_regex
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

          it "returns 400", :validation do
            put(request_url, platform.superuser,
              :payload => request_body).should look_like({
                :status => 400
              })
          end
        end

        context "without email but with external auth enabled" do
          let(:request_body) do
            {
              "username" => username,
              "display_name" => username,
              "external_authentication_uid" => username
            }
          end
          it "returns 200" do
            put(request_url, platform.superuser,
              :payload => request_body).should look_like({
                :status => 200
              })
          end
        end
        context "without username" do
          let(:request_body) do
            {
              "email" => "#{username}@chef.io",
              "first_name" => username,
              "last_name" => username,
              "display_name" => username,
              "password" => "badger badger"
            }
          end

          it "returns 400", :validation do
            put(request_url, platform.superuser,
              :payload => request_body).should look_like({
                :status => 400
              })
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

          it "returns 400", :validation do
            put(request_url, platform.superuser,
              :payload => request_body).should look_like({
                :status => 400
              })
          end
        end

        context "with spaces in names" do
          let(:request_body) do
            {
              "username" => username,
              "email" => "#{username}@chef.io",
              "first_name" => "Ren Kai",
              "last_name" => "de Boers",
              "display_name" => username,
              "password" => "badger badger"
            }
          end

          let(:modified_user) do
            {
              "username" => username,
              "email" => "#{username}@chef.io",
              "first_name" => "Ren Kai",
              "last_name" => "de Boers",
              "display_name" => username,
              "public_key" => public_key_regex
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
              "email" => "#{username}@chef.io",
              "first_name" => username,
              "last_name" => username,
              "display_name" => "some user",
              "password" => "badger badger"
            }
          end

          let(:modified_user) do
            {
              "username" => username,
              "email" => "#{username}@chef.io",
              "first_name" => username,
              "last_name" => username,
              "display_name" => "some user",
              "public_key" => public_key_regex
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
              "email" => "#{username}@chef.io",
              "first_name" => username,
              "last_name" => username,
              "display_name" => "ギリギリ",
              "password" => "badger badger"
            }
          end

          let(:modified_user) do
            {
              "username" => username,
              "email" => "#{username}@chef.io",
              "first_name" => username,
              "last_name" => username,
              "display_name" => "ギリギリ",
              "public_key" => public_key_regex
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
              "email" => "#{username}@chef.io",
              "first_name" => "Eliška",
              "last_name" => "Horáčková",
              "display_name" => username,
              "password" => "badger badger"
            }
          end

          let(:modified_user) do
            {
              "username" => username,
              "email" => "#{username}@chef.io",
              "first_name" => "Eliška",
              "last_name" => "Horáčková",
              "display_name" => username,
              "public_key" => public_key_regex
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

        context "with new password provided" do
          let(:request_body) do
            {
              "username" => username,
              "email" => "#{username}@chef.io",
              "first_name" => username,
              "last_name" => username,
              "display_name" => "new name",
              "password" => "bidgerbidger"
            }
          end
          it "changes the password" do
            put_response = put(request_url, platform.superuser, :payload => request_body)
            put_response.should look_like({ :status => 200 })

            response = post("#{platform.server}/authenticate_user", platform.superuser,
                            :payload => { 'username' => username, 'password' => 'bidgerbidger' })
            JSON.parse(response.body)["status"].should eq("linked")

          end
        end

        context "with public key provided" do
          let(:request_body) do
            {
              "username" => username,
              "email" => "#{username}@chef.io",
              "first_name" => username,
              "last_name" => username,
              "display_name" => "new name",
              "password" => "badger badger",
              "public_key" => input_public_key
            }
          end

          it "accepts the public key and subsequently responds with it" do
            put_response = put(request_url, platform.superuser, :payload => request_body)
            put_response.should look_like({
                                            :status => 200,
                                            :body=> {
                                              "uri" => request_url
                                            },
                                          })
            get_response = get(request_url, platform.superuser)
            new_public_key = JSON.parse(get_response.body)["public_key"]
            new_public_key.should eq(input_public_key)
          end
        end

        context "when a new private key is requested" do
          let(:request_body) do
            {
              "username" => username,
              "email" => "#{username}@chef.io",
              "first_name" => username,
              "last_name" => username,
              "display_name" => "new name",
              "password" => "badger badger",
              "private_key" => true
            }
          end

          it "can be used to successfully authenticate request" do
            put_response = put(request_url, platform.superuser, :payload => request_body)
            put_response.should look_like({
                                            :status => 200,
                                            :body_exact => {
                                              "uri" => request_url,
                                              "private_key" => private_key_regex
                                            },
                                          })

            new_private_key = JSON.parse(put_response.body)["private_key"]
            org_user = Pedant::User.new(username, new_private_key, {:associate => true,
                                                                    :preexisting => true })
            platform.make_user(org_user, platform.test_org)
            get(api_url("users"), org_user).should look_like({
                :status => 200
            })
          end
        end

        context "with private_key = true" do
          let(:request_body) do
            {
              "username" => username,
              "email" => "#{username}@chef.io",
              "first_name" => username,
              "last_name" => username,
              "display_name" => "new name",
              "password" => "badger badger",
              "private_key" => true
            }
          end

          it "returns a new private key, changes the public key" do
            original_response = get(request_url, platform.superuser)
            original_public_key = JSON.parse(original_response.body)["public_key"]

            put_response = put(request_url, platform.superuser, :payload => request_body)
            put_response.should look_like({
                                            :status => 200,
                                            :body_exact => {
                                              "uri" => request_url,
                                              "private_key" => private_key_regex
                                            },
                                          })

            new_private_key = JSON.parse(put_response.body)["private_key"]
            new_private_key.should_not eq(@original_private_key)

            new_response = get(request_url, platform.superuser)
            new_public_key = JSON.parse(new_response.body)["public_key"]
            new_public_key.should_not eq(original_public_key)
          end
        end



        context "and a public_key is present" do
          let(:request_body) do
            {
              "username" => username,
              "email" => "#{username}@chef.io",
              "first_name" => username,
              "last_name" => username,
              "display_name" => "new name",
              "password" => "badger badger",
              "private_key" => true,
              "public_key" => input_public_key
            }
          end

          it "returns a new private key, changes the public key" do
            original_response = get(request_url, platform.superuser)
            original_public_key = JSON.parse(original_response.body)["public_key"]

            put_response = put(request_url, platform.superuser, :payload => request_body)
            put_response.should look_like({
                                            :status => 200,
                                            :body_exact => {
                                              "uri" => request_url,
                                              "private_key" => private_key_regex
                                            },
                                          })

            new_private_key = JSON.parse(put_response.body)["private_key"]
            new_private_key.should_not eq(@original_private_key)

            new_response = get(request_url, platform.superuser)
            new_public_key = JSON.parse(new_response.body)["public_key"]

            new_public_key.should_not eq(input_public_key)
            new_public_key.should_not eq(original_public_key)
          end
        end

        context "and public key is present containing a certificate" do
          let(:request_body) do
            {
              "username" => username,
              "email" => "#{username}@chef.io",
              "first_name" => username,
              "last_name" => username,
              "display_name" => "new name",
              "password" => "badger badger",
              "private_key" => true,
              "public_key" => input_certificate
            }
          end

          it "returns a new private key, ignores the certificate" do
            original_response = get(request_url, platform.superuser)
            original_public_key = JSON.parse(original_response.body)["public_key"]

            put_response = put(request_url, platform.superuser, :payload => request_body)
            put_response.should look_like({
                                            :status => 200,
                                            :body_exact => {
                                              "uri" => request_url,
                                              "private_key" => private_key_regex
                                            },
                                          })

            new_private_key = JSON.parse(put_response.body)["private_key"]
            new_private_key.should_not eq(@original_private_key)

            new_response = get(request_url, platform.superuser)
            new_public_key = JSON.parse(new_response.body)["public_key"]

            new_public_key.should_not eq(input_certificate)
            new_public_key.should_not eq(original_public_key)
          end
        end

        context "with a certificate in the public_key field" do
          let(:request_body) do
            {
              "username" => username,
              "email" => "#{username}@chef.io",
              "first_name" => username,
              "last_name" => username,
              "display_name" => "new name",
              "password" => "badger badger",
              "public_key" => input_certificate
            }
          end

          it "returns 400", :validation do
            response = put(request_url, platform.superuser, :payload => request_body)
            response.should look_like({
                                        :status => 400,
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
              "email" => "#{username}@chef.io",
              "first_name" => username,
              "last_name" => username,
              "display_name" => username,
              "password" => "badger badger"
            }
          end

          let(:modified_user) do
            {
              "username" => new_name,
              "email" => "#{username}@chef.io",
              "first_name" => username,
              "last_name" => username,
              "display_name" => username,
              "public_key" => public_key_regex
            }
          end

          after :each do
            delete("#{platform.server}/users/#{new_name}", platform.superuser)
          end

          context "and the username is valid" do
            # Ideally these would be discrete tests: can we put it and get the correct response?
            # But the top-level PUT /users/:id context causes us some problems with it's before :each
            # behavior of recreating users.
            it "updates the user to the new name and provides a new uri" do
              put(request_url, platform.superuser,
                :payload => request_body).should look_like({
                  :status => 201,
                  :body_exact => { "uri" => new_request_url },
                  :headers => [ "Location" => new_request_url ]
                })

              # it "makes the user unavailable at the old URI"
              get(request_url, platform.superuser).should look_like({
                  :status => 404
                })
              # it "makes the user available at the new URI"
              get(new_request_url, platform.superuser).should look_like({
                  :status => 200,
                  :body_exact => modified_user
                })
            end
          end
        end

        context "changing username with UTF-8" do
          let(:new_name) { "テスト-#{Time.now.to_i}-#{Process.pid}" }

          let(:request_body) do
            {
              "username" => new_name,
              "email" => "#{username}@chef.io",
              "first_name" => username,
              "last_name" => username,
              "display_name" => username,
              "password" => "badger badger"
            }
          end

          it "returns 400", :validation do
            put(request_url, platform.superuser,
              :payload => request_body).should look_like({
                :status => 400
              })
            # it "does not process any change to username" do
            get(request_url, platform.superuser).should look_like({
                :status => 200
              })
          end
        end

        context "changing username with spaces" do
          let(:new_name) { "test #{Time.now.to_i}-#{Process.pid}" }

          let(:request_body) do
            {
              "username" => new_name,
              "email" => "#{username}@chef.io",
              "first_name" => username,
              "last_name" => username,
              "display_name" => username,
              "password" => "badger badger"
            }
          end

          it "returns 400", :validation do
            put(request_url, platform.superuser, :payload => request_body).should look_like({
                :status => 400
              })
            # it "does not process any change to username" do
            get(request_url, platform.superuser).should look_like({
                :status => 200
              })
          end
        end

        context "changing username with capital letters" do
          let(:new_name) { "Test-#{Process.pid}" }

          let(:request_body) do
            {
              "username" => new_name,
              "email" => "#{username}@chef.io",
              "first_name" => username,
              "last_name" => username,
              "display_name" => username,
              "password" => "badger badger"
            }
          end

          it "returns 400", :validation do
            put(request_url, platform.superuser,
              :payload => request_body).should look_like({
                :status => 400
              })
            # it "does not process any change to username" do
            get(request_url, platform.superuser).should look_like({
                :status => 200
              })
          end
        end


        context "new name already exists" do
          let(:request_body) do
            {
              "username" => platform.non_admin_user.name,
              "email" => "#{username}@chef.io",
              "first_name" => username,
              "last_name" => username,
              "display_name" => username,
              "password" => "badger badger"
            }
          end

          let(:unmodified_user) do
            {
              "username" => username,
              "email" => "#{username}@chef.io",
              "first_name" => username,
              "last_name" => username,
              "display_name" => username,
              "public_key" => public_key_regex
            }
          end

          it "returns 409" do
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
      end # context renaming users
    end # context PUT /users/<name>

    context "POST /users/<name>" do
      context "admin user" do
        it "returns  405" do
          post(request_url, platform.admin_user).should look_like({
              :status => 405
            })
        end
      end
    end # context POST /users/<name>

    context "DELETE /users/<name>" do
      let(:username) { "test-#{Process.pid}" }

      before :each do
        post("#{platform.server}/users", platform.superuser,
          :payload => {
            "username" => username,
            "email" => "#{username}@chef.io",
            "first_name" => username,
            "last_name" => username,
            "display_name" => username,
            "password" => "badger badger"
          }).should look_like({
            :status => 201,
            :body_exact => {
              "uri" => "#{platform.server}/users/#{username}",
              "private_key" => private_key_regex
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
          # Similar to rename, the existing before :each interferese with making this into a separate test
          # because it recreates the user.
          # it "did delete the user"
          get(request_url, platform.superuser).should look_like({
              :status => 404
          })
        end

      end

      context "admin user" do
        it "returns 403", :authorization do
          delete(request_url, platform.admin_user).should look_like({
              :status => 403
            })
          # it "did not delete user" do
          get("#{platform.server}/users/#{username}",
            platform.superuser).should look_like({
              :status => 200
            })
        end
      end

      context "default client" do
        it "returns 401", :authentication do
          delete(request_url, platform.non_admin_client).should look_like({
              :status => 401
            })
          # it "did not delete user" do
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
