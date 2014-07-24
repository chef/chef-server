# -*- coding: utf-8 -*-
require 'pedant/rspec/common'

# TODO
# Tests to write
#
# * multiple live invites in org work and are distinct (Low value for now)
# * multiple live invites for user work and are distinct (Low value for now)
# * Disassociation cleans up groups (needs better group fetch instrumentation)

describe "opscode-account user association", :association do
  let(:users_url) { "#{platform.server}/users" }
  let(:org_assoc_url) { api_url("association_requests") }
  let(:default_users_body)        { default_pedant_user_names.map { |user| {"user" => {"username" => user} } } }
  let(:default_pedant_user_names) { platform.users.select(&:associate).map(&:name).sort }

  def ruby_org_assoc?
    true
  end

  let(:public_key_regex) do
    # Because of a difference in the OpenSSL library between ruby 1.8.7
    # (actually 1.9.2) and 1.9.3, we have to accept multiple patterns here:
    /^-----BEGIN (RSA )?PUBLIC KEY-----/
  end


  # This embodies some assumptions around how multitenant config is done. Specifically, the normal
  # multitenant org setup process creates an org named 'pedant-testorg-PID'. Other variants (those
  # expecting a precreated org) would need a second org for the purpose, but that needs a bunch of
  # other work as well
  let(:other_org) { "pedant-otherorg-#{Process.pid}" }
  before(:all) do
    puts "Creating organization #{other_org} as part of association tests"
    platform.create_org(other_org)
  end
  after(:all) do
    platform.delete_org(other_org)
  end

  def make_user_assoc_url(user)
    "#{platform.server}/users/#{user}/association_requests"
  end
  def make_user_assoc_url_id(user, id)
    "#{platform.server}/users/#{user}/association_requests/#{id}"
  end
  def make_invite_payload(user)
    name = user.respond_to?(:name) ? user.name : user
    return { "user" => name }
  end

  def no_invites_for_user(user)
    get(make_user_assoc_url(user.name), user).should look_like({
                                                               :status => 200,
                                                               :body_exact=> []
                                                             })
  end

  def no_invites_for_org
    get(org_assoc_url, platform.admin_user).should look_like({
                                                               :status => 200,
                                                               :body_exact=> []
                                                             })
  end


  # todo this should be able to handle multiple outstanding invites
  def check_invite_for_user(user, invite_id)
    get(user_assoc_url, user).should look_like(
      {
         :status => 200,
         :body_exact=> [{
                          "id"=>invite_id,
                          "orgname"=>platform.test_org.name
                        }]
       })
    get(org_assoc_url, platform.admin_user).should look_like(
      {
         :status => 200,
         :body_exact=> [{
                          "id"=>invite_id,
                          "username"=>user.name
                        }]
       })

  end

  def accept_invite(user, orgname, invite_id)
    user_invite_url = make_user_assoc_url_id(user.name, invite_id)
    put(user_invite_url, user, :payload=>{:response=>"accept"}).should look_like({
                                                                                   :status => 200,
                                                                                   :body => {"organization"=>{"name"=>orgname} } })
  end

  def invite_user(orgname, username, inviter)
    post("#{platform.server}/organizations/#{orgname}/association_requests", inviter, :payload=>make_invite_payload(username)) do |response|
      response.should look_like({
                                  :status => 201,
                                  :body=>
                                  {"organization_user"=>{"username"=>inviter.name},
                                    "organization"=>{"name"=>orgname},
                                    "user"=>{"first_name"=>username}}
                                })
      return parse(response)['uri'].split('/')[-1]
    end
    return nil
  end

  def cleanup_requests_for_org
    org_assoc_url = api_url("association_requests")
    get(org_assoc_url, platform.admin_user) do |response|
      parse(response).each do |invite|
        id = invite["id"]
        delete(api_url("association_requests/#{id}"), platform.admin_user)
      end
    end
    no_invites_for_org
  end

  def cleanup_requests_for_user(user)
    url = make_user_assoc_url(user.name)
    get(url, user) do |response|
      parse(response).each do |invite|
        id = invite["id"]
        put("#{url}/#{id}", user, :payload=>{:response=>"reject"})
      end
    end
    no_invites_for_user(user)
  end

  context "starting state is sane" do
    let(:bad_user) { platform.bad_user.name }
    let(:user_assoc_url) { "#{users_url}/#{bad_user}/association_requests" }
    let(:user_assoc_count_url) { "#{users_url}/#{bad_user}/association_requests/count" }

    it "should start with no outstanding requests" do
      get(org_assoc_url, platform.admin_user).should look_like({
                                                                 :status => 200,
                                                                 :body_exact=> []
                                                               })
    end

    context "a user not in the org" do

      it "has a count of 0 invites" do
        get(user_assoc_count_url, platform.bad_user).should look_like({
                                                                        :status => 200,
                                                                        :body_exact=> {
                                                                          "value" => 0}
                                                                      })
      end

      it "invite list is empty" do

        get(user_assoc_url, platform.bad_user).should look_like({
                                                                  :status => 200,
                                                                  :body_exact=> []
                                                                })
      end
    end
  end

  context "user already in an org" do
    let(:user_assoc_url) { "#{users_url}/#{platform.non_admin_user.name}/association_requests" }
    it "cannot be invited" do

      post(org_assoc_url, platform.admin_user,
           :payload=>make_invite_payload(platform.non_admin_user)).should look_like( {


        :status => 409 })

      no_invites_for_user(platform.non_admin_user)

    end
  end


  context "user not in org" do
    let(:bad_user) { platform.bad_user.name }
    let(:user_assoc_url) { "#{users_url}/#{bad_user}/association_requests" }
    let(:user_assoc_count_url) { "#{users_url}/#{bad_user}/association_requests/count" }

    before :each do
      no_invites_for_org
      no_invites_for_user(platform.bad_user)
    end

    after :each do
      cleanup_requests_for_user(platform.bad_user)
      cleanup_requests_for_org
    end

    it "cannot invite itself to that org" do
      post(org_assoc_url, platform.bad_user, :payload=>make_invite_payload(bad_user)).should look_like({
                                                                                          :status => 403 })

      no_invites_for_user(platform.bad_user)
    end

    it "cannot be invited by non-admin user in the org" do
      post(org_assoc_url, platform.non_admin_user, :payload=>make_invite_payload(bad_user)).should look_like({
                                                                                                :status => 403 })

      get(user_assoc_url, platform.bad_user).should look_like({
                                                                :status => 200,
                                                                :body_exact=> []
                                                              })
    end

    context "can be invited to the org by an admin", :smoke do
      let (:accept_response_body) do
        { "organization"=>{"name"=>platform.test_org.name}  }
      end


      it "and the invite can be rescinded" do
        invite_id = invite_user(platform.test_org.name, bad_user, platform.admin_user)
        check_invite_for_user(platform.bad_user, invite_id)
        delete("#{org_assoc_url}/#{invite_id}", platform.admin_user).should look_like({ :status => 200 })
        no_invites_for_user(platform.bad_user)
      end

      # overlaps with previous; DRY it up?
      it "and once an the invite is rescinded it can't be accepted." do
        invite_id = invite_user(platform.test_org.name, bad_user, platform.admin_user)
        check_invite_for_user(platform.bad_user, invite_id)
        delete("#{org_assoc_url}/#{invite_id}", platform.admin_user).should look_like({ :status => 200 })

        user_invite_url = make_user_assoc_url_id(bad_user, invite_id)
        put(user_invite_url, platform.bad_user, :payload=>{:response=>"accept"}).should look_like({
                                                                                                    :status => 404,
                                                                                                    :body => {"error"=>"Cannot find association request: #{invite_id}"}
                                                                                                  })
      end
      it "and once an the invite is rescinded it can't be rejected." do
        invite_id = invite_user(platform.test_org.name, bad_user, platform.admin_user)
        check_invite_for_user(platform.bad_user, invite_id)
        delete("#{org_assoc_url}/#{invite_id}", platform.admin_user).should look_like({ :status => 200 })

        user_invite_url = make_user_assoc_url_id(bad_user, invite_id)
        put(user_invite_url, platform.bad_user, :payload=>{:response=>"reject"}).should look_like({
                                                                                                    :status => 404,
                                                                                                    :body => {"error"=>"Cannot find association request: #{invite_id}"}
                                                                                                  })
      end

      it "and can't be invited twice" do
        invite_id = invite_user(platform.test_org.name, bad_user, platform.admin_user)
        check_invite_for_user(platform.bad_user, invite_id)

        post("#{platform.server}/organizations/#{platform.test_org.name}/association_requests", platform.admin_user, :payload=>make_invite_payload(bad_user)).should look_like({ :status => 409 })


        delete("#{org_assoc_url}/#{invite_id}", platform.admin_user).should look_like({ :status => 200 })
        no_invites_for_user(platform.bad_user)
      end


      it "and can reject an invite" do
        invite_id = invite_user(platform.test_org.name, bad_user, platform.admin_user)

        user_invite_url = make_user_assoc_url_id(bad_user, invite_id)
        put(user_invite_url, platform.bad_user, :payload=>{:response=>"reject"}).should look_like({
                                                                                                    :status => 200,
                                                                                                    :body => accept_response_body })

        no_invites_for_user(platform.bad_user)

        get(api_url("users/#{bad_user}"), platform.bad_user).should look_like({
                                                                                :status=> 403,
                                                                                :body => {"error"=>
                                                                                  "'#{bad_user}' not associated with organization '#{platform.test_org.name}'"} })

        #can't accept after rejecting
        put(user_invite_url, platform.bad_user, :payload=>{:response=>"accept"}).should look_like({
                                                                                                    :status => 404,
                                                                                                    :body => {"error"=>"Cannot find association request: #{invite_id}"}
                                                                                                  })

      end

      it "and can accept an invite" do
        invite_id = invite_user(platform.test_org.name, bad_user, platform.admin_user)

        user_invite_url = make_user_assoc_url_id(bad_user, invite_id)
        put(user_invite_url, platform.bad_user, :payload=>{:response=>"accept"}).should look_like({
                                                                                                    :status => 200,
                                                                                                    :body => accept_response_body })

        no_invites_for_user(platform.bad_user)

        get(api_url("users/#{bad_user}"), platform.bad_user).should look_like({
                                                                                :status=> 200,
                                                                                :body => {} })

        # TODO - split this out. Testing for acceptance into an org is unrelated to testing for
        # removal from an org .
        delete(api_url("users/#{bad_user}"), platform.admin_user).should look_like({
                                                                                     :status=> 200 })

        # TODO Wrong test here with wrong atuh user - we want to make sure that the user is no longer in the org!
        get(api_url("users/#{bad_user}"), platform.bad_user).should look_like({
                                                                                :status=> 403,
                                                                                :body => {} })
      end

      it "and get set up properly" do
        invite_id = invite_user(platform.test_org.name, bad_user, platform.admin_user)
        user_invite_url = make_user_assoc_url_id(bad_user, invite_id)
        put(user_invite_url, platform.bad_user, :payload=>{:response=>"accept"}).should look_like({
                                                                                                    :status => 200,
                                                                                                    :body => accept_response_body })


        # check that they are a member of the org
        get(api_url("users/#{bad_user}"), platform.bad_user).should look_like({
                                                                                :status=> 200,
                                                                                :body => {"username"=>bad_user} })
        # check that the admin user can see them
        get(api_url("users/#{bad_user}"), platform.admin_user).should look_like({
                                                                                  :status=> 200,
                                                                                  :body => {"username"=>bad_user} })

        # check that they are in the users group. This isn't pretty because of USAGS
        # so we iterate through all groups in the users group to find the group containing only the
        # user
        groups = parse(get(api_url("groups/users"), platform.admin_user))
        result = false
        groups["groups"].each do |groupname|
          # probably should figure out if this is a usag or not before fetching it
          group = parse(get(api_url("groups/#{groupname}"), platform.admin_user))
          if group["actors"] == [bad_user]
            result = true
            break
          end
        end
        result.should be_true

        delete(api_url("users/#{bad_user}"), platform.admin_user).should look_like({
                                                                                     :status=> 200 })

      end

      #
      # Sometimes invites only fail when the user is already in an org. This can happen if we we
      # can't reverse-map global-admins in authz. Adding another global admin to the user read ace
      # then fails. Association is nearly correct, except that
      # 1) the user record can't be read by the admin
      # 2) The invite is never deleted
      #
      describe "belonging to another org as well" do
        it "can accept an invite, even when they belong to another org" do

          invite_id = invite_user(other_org, platform.bad_user.name, platform.superuser)
          accept_invite(platform.bad_user, other_org, invite_id)

          invite_id_2 = invite_user(platform.test_org.name, platform.bad_user.name, platform.admin_user)
          accept_invite(platform.bad_user, platform.test_org.name, invite_id_2)

          no_invites_for_user(platform.bad_user)

          get(api_url("users/#{bad_user}"), platform.bad_user).should
            look_like({ :status=> 200,
                        :body => {"username"=>bad_user} })


          delete(api_url("users/#{bad_user}"), platform.admin_user).should look_like({
                                                                                       :status=> 200 })

        end
        before(:each) do
          cleanup_requests_for_user(platform.bad_user)
        end
        after(:each) do
          delete("#{platform.server}/organizations/#{other_org}/users/#{bad_user}", platform.superuser)
          cleanup_requests_for_user(platform.bad_user)
        end
      end
    end
  end
  context "/organizations/<org>/users endpoint" do
    let(:request_url) { api_url("users") }

    context "GET /organizations/<org>/users" do
      let(:users_body) { default_users_body }

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
        it "returns  404[ruby]/405[erlang]" do
          put(request_url, platform.admin_user).should look_like({
              :status => ruby_org_assoc? ? 404 : 405
            })
        end
      end
    end # context PUT /organizations/<org>/users

    context "POST /organizations/<org>/users" do
      context "admin user" do
        # A 405 here would be fine (and is no doubt coming with erlang)
        it "returns  404[ruby]/405[erlang]" do
          post(request_url, platform.admin_user).should look_like({
              :status => ruby_org_assoc? ? 404 : 405
            })
        end
      end
    end # context POST /organizations/<org>/users

    context "DELETE /organizations/<org>/users" do
      context "admin user" do
        # A 405 here would be fine (and is no doubt coming with erlang)
        it "returns  404[ruby]/405[erlang]" do
          delete(request_url, platform.admin_user).should look_like({
              :status => ruby_org_assoc? ? 404 : 405
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
          "public_key" => public_key_regex
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
        it "returns  404[ruby]/405[erlang]" do
          put(request_url, platform.admin_user).should look_like({
              :status => ruby_org_assoc? ? 404 : 405
            })
        end
      end
    end # context PUT /organizations/<org>/users/<name>

    context "POST /organizations/<org>/users/<name>" do
      context "admin user" do
        # TODO this will be acceptable, but we must test for:
        #   - only superuser
        #   - user is associated if requested by superuser.
        # A 405 here would be fine (and is no doubt coming with erlang)
        it "returns  404[ruby]/405[erlang]" do
          post(request_url, platform.admin_user).should look_like({
              :status => ruby_org_assoc? ? 404 : 405
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
              :body_exact => default_users_body })
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
                  {"user" => {"username" => platform.admin_user.name}},
                  {"user" => {"username" => platform.non_admin_user.name}},
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
                  {"user" => {"username" => platform.admin_user.name}},
                  {"user" => {"username" => platform.non_admin_user.name}},
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
              :body_exact => default_users_body + [ {"user" => {"username" => username}} ]})
        end
      end
    end # context DELETE /organizations/<org>/users/<name>
  end # context /organizations/<org>/users/<name>
end
