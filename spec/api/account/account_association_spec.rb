# -*- coding: utf-8 -*-
require 'pedant/rspec/common'

# TODO
# Tests to write
#
# * multiple live invites in org work and are distinct (Low value for now)
# * multiple live invites for user work and are distinct (Low value for now)
# * Disassociation cleans up groups (needs better group fetch instrumentation)
#
# TODO
#  * time allowing, let's modify the association tests to use temporary users,
#  instead of platform.bad_user, so that if something fails in disassociating
#  we don't get false results based on platform.bad_user being given access
#  to something it shouldn't have
#  Use of platform.bad_user can also be a source of pain in concurrent runs
#
describe "opscode-account user association", :association do
  def self.ruby?
    true
  end

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
  shared(:other_org) { "pedant-otherorg-#{Process.pid}" }
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
  def make_org_assoc_requests_url(orgname)
    "#{platform.server}/organizations/#{orgname}/association_requests"
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
    result = put(user_invite_url, user, :payload=>{:response=>"accept"})
    result.should look_like( { :status => 200, :body => {"organization"=>{"name"=>orgname} } })
  end

  def invite_user(orgname, username, inviter)
    post("#{platform.server}/organizations/#{orgname}/association_requests", inviter,
         :payload=>make_invite_payload(username)) do |response|
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
      response = get(org_assoc_url, platform.admin_user)
      response.should look_like({ :status => 200, :body_exact=> [] })
    end

    context "a user not in the org" do
      it "has a count of 0 invites" do
        response = get(user_assoc_count_url, platform.bad_user)
        response.should look_like({ :status => 200, :body_exact=> { "value" => 0} })
      end

      it "invite list is empty" do
        response = get(user_assoc_url, platform.bad_user)
        response.should look_like({ :status => 200, :body_exact=> [] })
      end
    end
  end

  context "attempting to get association requests for a user that does not exist" do
    let(:user_assoc_url) { "#{users_url}/flappy/association_requests" }
    it "cannot find association list" do
      response = get(user_assoc_url, platform.superuser)
      response.should look_like({ :status => 404,
                                  :body_exact => { "error"=>"Could not find user flappy"} })
    end
    it "cannot found association count" do
      response = get("#{user_assoc_url}/count", platform.superuser)
      response.should look_like({ :status => 404,
                                  :body_exact => { "error"=>"Could not find user flappy"} })
    end
  end

  context "when org admin is attempting to view user associations" do
    let(:user_assoc_url) { "#{users_url}/#{platform.bad_user.name}/association_requests" }
    it "does not permit admin to view associations" do
      response = get(user_assoc_url, platform.admin_user)
      response.should look_like({ :status => 403,
                                  :body_exact => { "error"=>"You are not allowed to view association requests for #{platform.bad_user.name}"} })

    end
    it "does not permit admin to view association count" do
      response = get("#{user_assoc_url}/count", platform.admin_user)
      response.should look_like({ :status => 403,
                                  :body_exact => { "error"=>"You are not allowed to view association requests for #{platform.bad_user.name}"} })

    end
  end

  context "when the organization does not exist" do
    it "listing association requests replies with org not found" do
      pending("ruby incorrectly fails this with a 400", :if => ruby?) do
        response = get("/organizations/bad_org/association_requests", platform.superuser)
        response.should look_like({ :status => 404,
                                    :body_exact => { "error"=>"Organization bad_org not found."} })
      end
    end
    it "creating a new association request replies org not found" do
      pending("ruby incorrectly fails this with a 400", :if => ruby?) do
        response = post("/organizations/bad_org/association_requests", platform.superuser,
                        :payload=>make_invite_payload(platform.bad_user.name))
        response.should look_like({ :status => 404,
                                    :body_exact => { "error"=>"Organization bad_org not found."} })
      end
    end
  end

  context "listing association requests" do
    let(:user_assoc_url) { "#{users_url}/#{platform.bad_user.name}/association_requests" }
    let(:user_assoc_count_url) { "#{users_url}/#{platform.bad_user.name}/association_requests/count" }
    it "fails with 404 if the organization does not exist" do
      response = get(make_org_assoc_requests_url("badorgname"), platform.superuser)
      response.should look_like({ :status => 404, :body_exact => {  "error" => "Cannot find organization badorgname" } })
    end
    context "and association requests exist" do
      before  do
        @invite_id = invite_user(platform.test_org.name, platform.bad_user.name, platform.admin_user)
      end
      after do
        delete(api_url("association_requests/#{@invite_id}"), platform.admin_user)
        no_invites_for_user(platform.bad_user)
      end

      it "returns list of associations for the organization" do
        response = get(org_assoc_url, platform.admin_user)
        response.should look_like({ :status => 200,
                                    :body_exact=> [{ "id"=>@invite_id, "username"=>platform.bad_user.name }]
       })
      end
      it "returns list of associations for the user" do
        response = get(user_assoc_url, platform.bad_user)
        response.should look_like({ :status => 200,
                                    :body_exact=> [{ "id"=>@invite_id, "orgname"=>platform.test_org.name}]
       })
      end
      it "returns the correct count of associations for the user" do
        response = get(user_assoc_count_url, platform.bad_user)
        response.should look_like({ :status => 200, :body_exact=> { "value" => 1} })
      end
    end

    context "and no association requests exist" do
      before do
        # Sanity checks
        no_invites_for_org
        no_invites_for_user(platform.bad_user)
      end
      it "returns an empty list of association requests for the organization" do
        response = get(org_assoc_url, platform.admin_user)
        response.should look_like({ :status => 200, :body_exact=> [] })
      end
      it "returns an empty list of association requests for the user" do
        response = get(user_assoc_url, platform.bad_user)
        response.should look_like({ :status => 200, :body_exact=> [] })
      end
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
      response = post(org_assoc_url, platform.bad_user, :payload=>make_invite_payload(bad_user))
      response.should look_like({ :status => 403, :body_exact => { "error" => "'#{bad_user}' not associated with organization '#{platform.test_org.name}'" }  })
      no_invites_for_user(platform.bad_user)
    end

    it "cannot be invited by non-admin user in the org" do
      response = post(org_assoc_url, platform.non_admin_user, :payload=>make_invite_payload(bad_user))
      response.should look_like({ :status => 403 })

      response = get(user_assoc_url, platform.bad_user)
      response.should look_like({ :status => 200, :body_exact=> [] })
    end

    context "can be invited to the org by an admin", :smoke do
      let(:accept_response_body) do
        { "organization" => { "name" => platform.test_org.name }  }
      end

      it "unless the user is already in the org" do
        invite_id = invite_user(platform.test_org.name, bad_user, platform.admin_user)
        check_invite_for_user(platform.bad_user, invite_id)
        user_invite_url = make_user_assoc_url_id(bad_user, invite_id)
        response = put(user_invite_url, platform.bad_user, :payload=>{:response=>"accept"})
        response = post(api_url("association_requests"),
                           platform.admin_user, :payload=>make_invite_payload(bad_user))

        response.should look_like( { :status => 409,
                                     :body_exact => { "error" => "The association already exists." } })
        no_invites_for_user(platform.non_admin_user)
        # Clean up
        response = delete(api_url("users/#{bad_user}"), platform.admin_user)
        response.should look_like({ :status=> 200 })
      end

      it "and the invite can be rescinded" do
        invite_id = invite_user(platform.test_org.name, bad_user, platform.admin_user)
        check_invite_for_user(platform.bad_user, invite_id)
        delete("#{org_assoc_url}/#{invite_id}", platform.admin_user).should look_like({ :status => 200 })
        no_invites_for_user(platform.bad_user)
      end

      it "and a rescinded invite can't be rescinded again" do
        invite_id = invite_user(platform.test_org.name, bad_user, platform.admin_user)
        delete("#{org_assoc_url}/#{invite_id}", platform.admin_user).should look_like({ :status => 200 })
        response = delete("#{org_assoc_url}/#{invite_id}", platform.admin_user)
        response.should look_like({ :status => 404, :body => {"error" => "Cannot find association request: #{invite_id}" } })
      end

      it "and once an the invite is rescinded it can't be accepted" do
        invite_id = invite_user(platform.test_org.name, bad_user, platform.admin_user)
        check_invite_for_user(platform.bad_user, invite_id)
        delete("#{org_assoc_url}/#{invite_id}", platform.admin_user).should look_like({ :status => 200 })

        user_invite_url = make_user_assoc_url_id(bad_user, invite_id)
        response = put(user_invite_url, platform.bad_user, :payload=>{:response=>"accept"})
        response.should look_like({ :status => 404, :body => {"error"=>"Cannot find association request: #{invite_id}"} })
      end

      it "and once an the invite is rescinded it can't be rejected." do
        invite_id = invite_user(platform.test_org.name, bad_user, platform.admin_user)
        check_invite_for_user(platform.bad_user, invite_id)
        delete("#{org_assoc_url}/#{invite_id}", platform.admin_user).should look_like({ :status => 200 })

        user_invite_url = make_user_assoc_url_id(bad_user, invite_id)
        response = put(user_invite_url, platform.bad_user, :payload=>{:response=>"reject"})
        response.should look_like({ :status => 404, :body => {"error"=>"Cannot find association request: #{invite_id}"} })
      end

      it "and can't be invited twice" do
        invite_id = invite_user(platform.test_org.name, bad_user, platform.admin_user)
        check_invite_for_user(platform.bad_user, invite_id)

        post("#{platform.server}/organizations/#{platform.test_org.name}/association_requests", platform.admin_user,
             :payload=>make_invite_payload(bad_user)).should look_like({ :status => 409 })


        delete("#{org_assoc_url}/#{invite_id}", platform.admin_user).should look_like({ :status => 200 })
        no_invites_for_user(platform.bad_user)
      end

      it "and can reject an invite" do
        invite_id = invite_user(platform.test_org.name, bad_user, platform.admin_user)

        user_invite_url = make_user_assoc_url_id(bad_user, invite_id)
        response = put(user_invite_url, platform.bad_user, :payload=>{:response=>"reject"})
        response.should look_like({ :status => 200, :body => accept_response_body })

        no_invites_for_user(platform.bad_user)

        response = get(api_url("users/#{bad_user}"), platform.bad_user)
        response.should look_like({ :status=> 403,
                                    :body => {"error"=> "'#{bad_user}' not associated with organization '#{platform.test_org.name}'"} })

        #can't accept after rejecting
        response = put(user_invite_url, platform.bad_user, :payload=>{:response=>"accept"})
        response.should look_like({ :status => 404,
                                    :body => {"error"=>"Cannot find association request: #{invite_id}"} })

      end

      it "deleting an invite via the /users path is not accepted" do
        invite_id = invite_user(platform.test_org.name, bad_user, platform.admin_user)
        user_invite_url = make_user_assoc_url_id(bad_user, invite_id)
        response = delete(user_invite_url, platform.bad_user)
        response.should look_like({ :status => ruby? ? 404 : 405 })

        # Cleanup -delete the invite through proper path.
        delete("#{org_assoc_url}/#{invite_id}", platform.admin_user).should look_like({ :status => 200 })
      end

      it "and can accept an invite" do
        invite_id = invite_user(platform.test_org.name, bad_user, platform.admin_user)

        user_invite_url = make_user_assoc_url_id(bad_user, invite_id)
        response = put(user_invite_url, platform.bad_user, :payload=>{:response=>"accept"})
        response.should look_like({ :status => 200, :body => accept_response_body })

        no_invites_for_user(platform.bad_user)
        response = get(api_url("users/#{bad_user}"), platform.bad_user)
        response.should look_like({ :status=> 200, :body => {} })

        # Clean up
        response = delete(api_url("users/#{bad_user}"), platform.admin_user)
        response.should look_like({ :status=> 200 })
      end

      context "after a user is deleted from an org" do
        before :each do
          invite_id = invite_user(platform.test_org.name, bad_user, platform.admin_user)
          user_invite_url = make_user_assoc_url_id(bad_user, invite_id)
          response = put(user_invite_url, platform.bad_user, :payload=>{:response=>"accept"})
          response.should look_like({ :status => 200, :body => accept_response_body })
          response = delete(api_url("users/#{bad_user}"), platform.admin_user)
          response.should look_like({ :status=> 200 })
        end

        it "they do not have access to view members of that org" do
          result = get(api_url("users"), platform.bad_user)
          result.should look_like({ :status=> 403,
                                    :body_exact => { "error" => "'#{bad_user}' not associated with organization '#{platform.test_org.name}'"} })

        end

        it "they do not have access to attempt to view themselves in an org" do
          result = get(api_url("users/#{bad_user}"), platform.bad_user)
          result.should look_like({ :status=> 403,
                                    :body_exact => { "error" => "'#{bad_user}' not associated with organization '#{platform.test_org.name}'"} })

        end
        it "an org admin no longer sees them in the org" do
          pending("ruby incorrectly fails this with a 403", :if => ruby?) do
            result = get(api_url("users/#{bad_user}"), platform.admin_user)
            result.should look_like({ :status=> 404,
                                      :body_exact => { "error" => "Cannot find a user #{bad_user} in organization #{platform.test_org.name}" } })
          end
        end
      end

      it "must either accept or reject an invite" do
        invite_id = invite_user(platform.test_org.name, bad_user, platform.admin_user)

        user_invite_url = make_user_assoc_url_id(bad_user, invite_id)
        response = put(user_invite_url, platform.bad_user, :payload=>{:response=>"blither"})
        response.should look_like({ :status => 400,
                                    :body_exact => {"error"=>"Param response must be either 'accept' or 'reject'"} })
        delete("#{org_assoc_url}/#{invite_id}", platform.admin_user).should look_like({ :status => 200 })
      end


      it "and get set up properly" do
        invite_id = invite_user(platform.test_org.name, bad_user, platform.admin_user)
        user_invite_url = make_user_assoc_url_id(bad_user, invite_id)
        response = put(user_invite_url, platform.bad_user, :payload=>{:response=>"accept"})
        response.should look_like({ :status => 200, :body => accept_response_body })


        # check that they are a member of the org
        response= get(api_url("users/#{bad_user}"), platform.bad_user)
        response.should look_like({ :status=> 200, :body => {"username"=>bad_user} })

        # check that the admin user can see them
        response = get(api_url("users/#{bad_user}"), platform.admin_user)
        response.should look_like({ :status=> 200, :body => {"username"=>bad_user} })

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

        delete(api_url("users/#{bad_user}"), platform.admin_user).should look_like({ :status=> 200 })

      end
      context "when admin is deleted" do
        let(:bad_user) { platform.bad_user.name }
        let(:org) { platform.test_org.name }
        let(:admin_username) { "test-admin-#{Time.now.to_i}-#{Process.pid}" }
        let(:test_admin_user) { platform.create_user(admin_username) }
        let(:test_username) { "test-user-#{Time.now.to_i}-#{Process.pid}" }
        let(:test_user) { platform.create_user(test_username) }
        let(:invalid_invite_msg) {
          "This invitation is no longer valid.  Please notify an adminstrator and request to be re-invited to the organization"
        }

        before :each do
          platform.associate_user_with_org(org, test_admin_user)
          platform.add_user_to_group(org, test_admin_user, "admins")
          invite_id = invite_user(org, test_user.name, test_admin_user)
          @user_invite_url = make_user_assoc_url_id(test_user.name, invite_id)
        end

        after :each do
          # User may have already been disassociated and/or deleted depending on the test,
          # but clean up here just in case.
          delete(api_url("users/#{test_user.name}"), platform.admin_user)
          delete(api_url("users/#{test_admin_user.name}"), platform.admin_user)

          delete("#{platform.server}/users/#{test_admin_user.name}", platform.superuser)
          delete("#{platform.server}/users/#{test_user.name}", platform.superuser)
        end

        it "from the org, invites issued by that admin cannot be accepted" do
          pending("unclear why this is presently passing w/ 200 on ruby, should not be", :if => ruby?) do
            delete(api_url("users/#{admin_username}"), platform.admin_user).should look_like({ :status=> 200 })
            response = put(@user_invite_url, test_user, :payload=>{:response=>"accept"})
            response.should look_like({ :status => 403,
                                        :body_exact => { "error" => invalid_invite_msg } })
            # Ensure invite was deleted
            no_invites_for_user(test_user)
          end
        end

        it "from the system, invites issued by that admin can't by accepted" do
          pending "Known failure: passes w/ 200 b/c no USAG cleanup performed for deleted user" do
            delete("/users/#{admin_username}", platform.superuser).should look_like({ :status => 200} )
            response = put(@user_invite_url, test_user, :payload=>{:response=>"accept"})
            response.should look_like({ :status => 403,
                                        :body_exact => { "error" => invalid_invite_msg } })
            # Ensure invite was deleted
            no_invites_for_user(test_user)
          end
        end
      end

      context "OC-11708 - when last updator of users group is dissociated" do
        let(:org) { platform.test_org.name }
        let(:admin_username) { "test-admin-#{Time.now.to_i}-#{Process.pid}" }
        let(:test_admin_user) { platform.create_user(admin_username) }
        let(:test_username) { "test-user-#{Time.now.to_i}-#{Process.pid}" }
        let(:test_user) { platform.create_user(test_username) }

        before(:each) do
          # add a test admin user to the org
          platform.associate_user_with_org(org, test_admin_user)
          platform.add_user_to_group(org, test_admin_user, "admins")

          # add and remove user from users group as the admin to ensure that the
          # last_updated_by field of the group record belongs to the admin user
          platform.add_user_to_group(org, test_admin_user, "users", test_admin_user)
          platform.remove_user_from_group(org, test_admin_user, "users", test_admin_user)

          # dissociate test-admin from the org, ensuring that the user no longer
          # has rights on the users group that they just updated
          # also, delete the user for safe measure and remove them from the admins
          # group
          platform.remove_user_from_group(org, test_admin_user, "admins", platform.superuser)
          delete(api_url("users/#{test_admin_user.name}"), platform.admin_user)
          delete("#{platform.server}/users/#{test_admin_user.name}", platform.superuser)

          # invite the user to the org, any admin / superuser is fine
          @invite_id = invite_user(org, test_user.name, platform.superuser)
        end

        after(:each) do
          # cleanup the users that we've created in the org
          delete(api_url("users/#{test_user.name}"), platform.admin_user)
          delete("#{platform.server}/users/#{test_user.name}", platform.superuser)
        end

        it "can accept the invite" do
          # the accept invite method validates the status and content of the
          # response
          accept_invite(test_user, org, @invite_id)
        end
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

          response = get(api_url("users/#{bad_user}"), platform.bad_user)
          response.should look_like({ :status=> 200, :body => {"username"=>bad_user} })

          response = delete(api_url("users/#{bad_user}"), platform.admin_user)

          response.should look_like({ :status => 200 } )
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
      it "as superuser returns  404 in ruby and 200 in erlang" do
        post(request_url, platform.superuser).should look_like({
            :status => ruby_org_assoc? ? 404 : 200
          })
      end
      it "as org admin user returns 404 in ruby and 403 in erlang" do
        post(request_url, platform.admin_user).should look_like({
            :status => ruby_org_assoc? ? 404 : 403
          })
      end
      it "as non-admin org user returns  404 in ruby and 200 in erlang" do
        post(request_url, platform.non_admin_user).should look_like({
            :status => ruby_org_assoc? ? 404 : 200
          })
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

      context "user acting on self" do
        context "when actor is also an org admin" do
          before { platform.add_user_to_group(org, test_user, "admins") }
          after  { platform.remove_user_from_group(org, test_user, "admins") }

          it "cannot delete" do
            pending("new constraint in erchef- ruby returns 200", :if => ruby?) do
              delete(request_url, test_user).should look_like({
                  :status => 403,
                  :body_exact => {"error" => "" }
                })
            end
          end
        end
        context "when actor is not an org admin" do
          it "can delete" do
            delete(request_url, test_user).should look_like({
                :status => 200
              })
          end
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

      context "OC-11708 - when last updator of users group is dissociated" do
        let(:admin_username) { "test-admin-#{Time.now.to_i}-#{Process.pid}" }
        let(:test_admin_user) { platform.create_user(admin_username) }

        before(:each) do
          platform.associate_user_with_org(org, test_admin_user)
          platform.add_user_to_group(org, test_admin_user, "admins")

          # add and remove user from users group as the admin to ensure that the
          # last_updated_by field of the group record belongs to the admin user
          platform.add_user_to_group(org, test_admin_user, "users", test_admin_user)
          platform.remove_user_from_group(org, test_admin_user, "users", test_admin_user)

          # dissociate test-admin from the org, ensuring that the user no longer
          # has rights on the users group that they just updated
          # also, delete the user for safe measure and remove them from the admins
          # group
          platform.remove_user_from_group(org, test_admin_user, "admins", platform.superuser)
          delete(api_url("users/#{test_admin_user.name}"), platform.admin_user)
          delete("#{platform.server}/users/#{test_admin_user.name}", platform.superuser)
        end

        context "as an admin user" do
          it "can delete" do
            delete(request_url, platform.admin_user).should look_like({
                                                              :status => 200
                                                            })
          end
        end

        context "as a user acting on self" do
          it "can delete" do
            delete(request_url, test_user).should look_like({
                                                              :status => 200
                                                            })
          end
        end
      end
    end # context DELETE /organizations/<org>/users/<name>
  end # context /organizations/<org>/users/<name>
end
