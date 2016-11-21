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
  # Generate a random 7-digit number
  def rand_id
    rand(10**7...10**8).to_s
  end

  let(:users_url) { "#{platform.server}/users" }
  let(:org_assoc_url) { api_url("association_requests") }
  let(:default_users_body)        { default_pedant_user_names.map { |user| {"user" => {"username" => user} } } }
  let(:default_pedant_user_names) { platform.users.select(&:associate).map(&:name).sort }

  let(:public_key_regex) do
     /^-----BEGIN (RSA )?PUBLIC KEY-----/
  end

  # Because in Chef 11 this is a string, but a list in Chef 12:
  let(:org_association_error) {
      ["'#{bad_user}' not associated with organization '#{platform.test_org.name}'"]
  }

  # This embodies some assumptions around how multitenant config is done. Specifically, the normal
  # multitenant org setup process creates an org named 'pedant-testorg-PID'. Other variants (those
  # expecting a precreated org) would need a second org for the purpose, but that needs a bunch of
  # other work as well
  let(:other_org) { "pedant-otherorg-#{Process.pid}" }
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
    expect(get(make_user_assoc_url(user.name), user)).to look_like({
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

  def user_should_be_in_org(user, requestor=nil)
    get(api_url("users/#{bad_user}"), requestor || platform.admin_user).should look_like({
                                                                :status=> 200,
                                                                :body => {"username"=>user.name} })

  end
  def user_should_not_be_in_org(user)
    response = get(api_url("users"), platform.admin_user)
    response.code.should == 200
    !parse(response).any? { |u| u['user']['username'] == user }
  end

  def user_should_be_in_group(user, group)
    is_user_in_group?(user, group).should be true
  end

  def is_user_in_group?(user, group)
    # check that they are in the users group. This isn't pretty because of USAGS
    # so we iterate through all groups in the users group to find the group containing only the
    # user
    group = parse(get(api_url("groups/#{group}"), platform.admin_user))
    result = group['actors'].include?(user.name)
    if !result
      result = group["groups"].any? { |child| is_user_in_group?(user, child) }
    end
    result
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
    url = "#{platform.server}/organizations/#{orgname}/association_requests"
    post(url, inviter, :payload=>make_invite_payload(username)) do |response|
      response.should look_like({ :status => 201 })
      uri = parse(response)['uri']
      uri.split('/')[0..-2].should == url.split('/')
      return uri.split('/')[-1]
    end
    return nil
  end

  def cleanup_requests_for_org(orgname)
      org_assoc_url = "#{platform.server}/organizations/#{orgname}/association_requests"
      get(org_assoc_url, platform.admin_user) do |response|
        parse(response).each do |invite|
          id = invite["id"]
          delete(api_url("association_requests/#{id}"), platform.superuser).should look_like({:status => 200})
        end
      end
  end

  def cleanup_requests_for_user(user)
    url = make_user_assoc_url(user.name)
    get(url, user) do |response|
      parse(response).each do |invite|
        id = invite["id"]
        expect(put("#{url}/#{id}", user, :payload=>{:response=>"reject"})).to look_like({ :status => 200 })
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

  context "/users/USER/organizations endpoint" do
    let(:test_username) { "test-user-#{rand_id}-#{Process.pid}" }
    let(:test_orgname2) { "test-org-#{rand_id}-#{Process.pid}" }
    let(:test_user) { platform.create_user(test_username) }
    let(:test_org2) { platform.create_org(test_orgname2)}
    let(:test_user2) { platform.create_user(test_username + "-2") }
    let(:user_org_url) { "#{users_url}/#{test_username}/organizations" }
    before do
      platform.create_org(test_orgname2)
      platform.associate_user_with_org(platform.test_org.name, test_user)
      platform.associate_user_with_org(platform.test_org.name, test_user2)
      platform.associate_user_with_org(test_orgname2, test_user2)
    end
    after do
      [test_username, "#{test_username}-2"].each do |u|
        delete(api_url("users/#{u}"), platform.superuser)
        delete("#{platform.server}/users/#{u}", platform.superuser)
      end
      delete("#{platform.server}/organizations/#{test_orgname2}", platform.superuser)
    end

    context "invoking" do
      %w{post put delete}.each do |method|
        it "#{method} fails appropriately" do
          result = send(method, user_org_url, platform.superuser, {})
          result.should look_like({ :status => 405 })
        end
      end

      context "GET" do
        context "when the requesting user is the targetuser" do
          it "returns all organizations the user is a member of" do
            result = get(user_org_url, test_user)
            result.should look_like({:status => 200})
            expect(JSON.parse(result).length == 2)
          end

          it "returns a proper list of orgs for the user" do
            result = get(user_org_url, test_user)
            result.should look_like({ :status => 200 })
            json = JSON.parse(result)
            org = json.find {|o| o["organization"]["name"] == platform.test_org.name }["organization"]
            expect(org["name"]).to eq(platform.test_org.name)
            expect(org["full_name"]).to eq(platform.test_org.name)
            expect(org["guid"].nil?).to be(false)
            expect(org["guid"].empty?).to be(false)
          end
        end

        context "when the requesting user is the superuser" do
          it "returns all organizations the user is a member of" do
            result = get(user_org_url, platform.superuser)
            result.should look_like({:status => 200})
            expect(JSON.parse(result).length == 2)
          end
        end

        context "when the requesting user shares 1 organization with the target user" do
          it "only returns the shared organization" do
            result = get(user_org_url, platform.superuser)
            result.should look_like({:status => 200})
            expect(JSON.parse(result).length).to eq(1)
            expect(JSON.parse(result)[0]["organization"]["name"]).to eq(platform.test_org.name)
          end
        end
      end
    end

    context "invoking as", :authorization do
      it "superuser succeeds" do
        get(user_org_url, platform.superuser).should look_like({:status => 200})
      end
      it "the user succeeds" do
        get(user_org_url, test_user).should look_like({:status => 200})
      end
      it "an admin org user succeeds" do
        get(user_org_url, platform.admin_user).should look_like({:status => 200})
      end
      it "another org member succeeds" do
        get(user_org_url, platform.non_admin_user).should look_like({:status => 200})
      end
      it "some other user fails" do
        get(user_org_url, platform.bad_user).should look_like({:status => 403})
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
    it "cannot find association count" do
      response = get("#{user_assoc_url}/count", platform.superuser)
      response.should look_like({ :status => 404,
                                  :body_exact => { "error"=>"Could not find user flappy"} })
    end
  end

  context "when superuser is attempting to view user associations" do
    let(:user_assoc_url) { "#{users_url}/#{platform.bad_user.name}/association_requests" }
    it "does permit superuser to view associations" do
      expect(get(user_assoc_url, platform.superuser)).to look_like({:status => 200})
    end
    it "does permit superuser to view association count" do
      expect(get("#{user_assoc_url}/count", platform.superuser)).to look_like({:status => 200})
    end
  end

  context "when org admin is attempting to view user associations", :authorization do
    let(:user_assoc_url) { "#{users_url}/#{platform.bad_user.name}/association_requests" }
    let(:read_fail_message) { ["missing read permission"] }
    it "does not permit admin to view associations", :authorization do
      response = get(user_assoc_url, platform.admin_user)
      response.should look_like({ :status => 403,
                                  :body_exact => { "error"=> read_fail_message} } )

    end
    it "does not permit admin to view association count", :authorization do
      response = get("#{user_assoc_url}/count", platform.admin_user)
      response.should look_like({ :status => 403,
                                  :body_exact => { "error"=> read_fail_message} } )
    end
  end

  context "user already in an org" do
    let(:user_assoc_url) { "#{users_url}/#{platform.non_admin_user.name}/association_requests" }
    it "cannot be invited by a non-admin", :authorization do
      post(org_assoc_url, platform.non_admin_user, :payload=>make_invite_payload(platform.non_admin_user)).should look_like({
                                                                                                                              :status => 403 })
      no_invites_for_user(platform.non_admin_user)
    end
  end

  context "when the organization does not exist" do
    let(:error_msg) { ["organization 'bad_org' does not exist."]}
    it "listing association requests replies with org not found" do
        response = get("#{platform.server}/organizations/bad_org/association_requests", platform.superuser)
        response.should look_like({ :status => 404,
                                    :body_exact => { "error"=> error_msg} })
    end
    it "creating a new association request replies org not found" do
        response = post("#{platform.server}/organizations/bad_org/association_requests", platform.superuser,
                        :payload=>make_invite_payload(platform.bad_user.name))
        response.should look_like({ :status => 404,
                                    :body_exact => { "error"=> error_msg} })
    end
  end

  context "listing association requests" do
    let(:user_assoc_url) { "#{users_url}/#{platform.bad_user.name}/association_requests" }
    let(:user_assoc_count_url) { "#{users_url}/#{platform.bad_user.name}/association_requests/count" }
    it "fails with 404 if the organization does not exist" do
      error_msg = ["organization 'badorgname' does not exist."]
      response = get(make_org_assoc_requests_url("badorgname"), platform.superuser)
      response.should look_like({ :status => 404, :body_exact => {  "error" => error_msg } })
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
      cleanup_requests_for_org(platform.test_org.name)
      no_invites_for_org
    end

    it "cannot invite itself to that org", :authorization do
      response = post(org_assoc_url, platform.bad_user, :payload=>make_invite_payload(bad_user))
      response.should look_like({ :status => 403, :body_exact => { "error" => org_association_error }  })
      no_invites_for_user(platform.bad_user)
    end

    it "cannot be invited by non-admin user in the org", :authorization do
      response = post(org_assoc_url, platform.non_admin_user, :payload => make_invite_payload(bad_user))
      response.should look_like({ :status => 403 })

      response = get(user_assoc_url, platform.bad_user)
      response.should look_like({ :status => 200, :body_exact=> [] })
    end

    context "can be invited to the org by an admin", :smoke do
      let(:accept_response_body) do
        { "organization" => { "name" => platform.test_org.name }  }
      end

      after(:each) do
        # Prevent data from one failure from polluting the next test.
        delete(api_url("users/#{bad_user}"), platform.admin_user)
        cleanup_requests_for_org(platform.test_org.name)
      end

      it "unless the user is already in the org" do
        platform.associate_user_with_org(platform.test_org.name, platform.bad_user)
        response = post(api_url("association_requests"),
                           platform.admin_user, :payload=>make_invite_payload(bad_user))
        response.should look_like( { :status => 409,
                                     :body_exact => { "error" => "The association already exists." } })
        no_invites_for_user(platform.non_admin_user)
      end

      it "and the invite can be rescinded" do
        invite_id = invite_user(platform.test_org.name, bad_user, platform.admin_user)
        check_invite_for_user(platform.bad_user, invite_id)
        delete("#{org_assoc_url}/#{invite_id}", platform.admin_user).should look_like({ :status => 200 })
        no_invites_for_user(platform.bad_user)
        user_should_not_be_in_org(platform.bad_user)
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
        user_should_not_be_in_org(platform.bad_user)
      end

      it "and once an the invite is rescinded it can't be rejected." do
        invite_id = invite_user(platform.test_org.name, bad_user, platform.admin_user)
        check_invite_for_user(platform.bad_user, invite_id)
        delete("#{org_assoc_url}/#{invite_id}", platform.admin_user).should look_like({ :status => 200 })

        user_invite_url = make_user_assoc_url_id(bad_user, invite_id)
        response = put(user_invite_url, platform.bad_user, :payload=>{:response=>"reject"})
        response.should look_like({ :status => 404, :body => {"error"=>"Cannot find association request: #{invite_id}"} })
        user_should_not_be_in_org(platform.bad_user)
      end

      it "and can't be invited twice" do
        invite_id = invite_user(platform.test_org.name, bad_user, platform.admin_user)
        check_invite_for_user(platform.bad_user, invite_id)
        msg = "The invite already exists."
        post("#{platform.server}/organizations/#{platform.test_org.name}/association_requests", platform.admin_user,
             :payload=>make_invite_payload(bad_user)).should look_like({ :status => 409,
                                                                         :body_exact => { "error" => msg } })

        delete("#{org_assoc_url}/#{invite_id}", platform.admin_user).should look_like({ :status => 200 })
        no_invites_for_user(platform.bad_user)
        user_should_not_be_in_org(platform.bad_user)
      end

      it "and can reject an invite" do
        invite_id = invite_user(platform.test_org.name, bad_user, platform.admin_user)

        user_invite_url = make_user_assoc_url_id(bad_user, invite_id)
        put(user_invite_url, platform.bad_user, :payload=>{:response=>"reject"}).should look_like({ :status => 200 })

        no_invites_for_user(platform.bad_user)

        #can't accept after rejecting
        response = put(user_invite_url, platform.bad_user, :payload=>{:response=>"accept"})
        response.should look_like({ :status => 404,
                                    :body => {"error"=>"Cannot find association request: #{invite_id}"} })

        user_should_not_be_in_org(platform.bad_user)
      end

      it "deleting an invite via the /users path is not accepted" do
        invite_id = invite_user(platform.test_org.name, bad_user, platform.admin_user)
        user_invite_url = make_user_assoc_url_id(bad_user, invite_id)
        response = delete(user_invite_url, platform.bad_user)
        response.should look_like({ :status => 405 })

        # Cleanup -delete the invite through proper path.
        delete("#{org_assoc_url}/#{invite_id}", platform.admin_user).should look_like({ :status => 200 })
      end

      it "and can accept an invite" do
        invite_id = invite_user(platform.test_org.name, bad_user, platform.admin_user)

        user_invite_url = make_user_assoc_url_id(bad_user, invite_id)
        response = put(user_invite_url, platform.bad_user, :payload=>{:response=>"accept"})
        response.should look_like({ :status => 200, :body => accept_response_body })
      end

      it "and can reject an invite, after which they cannot access things in the org", :authorization do
        invite_id = invite_user(platform.test_org.name, bad_user, platform.admin_user)

        user_invite_url = make_user_assoc_url_id(bad_user, invite_id)
        put(user_invite_url, platform.bad_user, :payload=>{:response=>"reject"}).should look_like({ :status => 200 })

        no_invites_for_user(platform.bad_user)

        #can't accept after rejecting
        response = put(user_invite_url, platform.bad_user, :payload=>{:response=>"accept"})
        response.should look_like({ :status => 404, :body => {"error"=>"Cannot find association request: #{invite_id}"} })

        user_should_not_be_in_org(platform.bad_user)

        response = get(api_url("users/#{bad_user}"), platform.bad_user)
        response.should look_like({ :status=> 403, :body => {"error"=> org_association_error} })
      end

      context "after a user is deleted from an org" do
        let(:requestor_not_in_org) { org_association_error }
        let(:target_not_found_in_org) { "Cannot find a user #{bad_user} in organization #{platform.test_org.name}" }
        before :each do
          platform.associate_user_with_org(platform.test_org.name, platform.bad_user)
          response = delete(api_url("users/#{bad_user}"), platform.admin_user)
          response.should look_like({ :status=> 200 })
        end

        it "they do not have access to view members of that org", :authorization do
          result = get(api_url("users"), platform.bad_user)
          result.should look_like({ :status=> 403,
                                    :body_exact => { "error" => requestor_not_in_org} })

        end

        it "they do not have access to attempt to view themselves in an org", :authorization do
          result = get(api_url("users/#{bad_user}"), platform.bad_user)
          result.should look_like({ :status=> 403,
                                    :body_exact => { "error" => requestor_not_in_org} })

        end
        it "an org admin no longer sees them in the org" do
          result = get(api_url("users/#{bad_user}"), platform.admin_user)
          result.should look_like({ :status=> 404,
                                    :body_exact => { "error" => target_not_found_in_org} })
        end
        it "admin attempting to delete this user from the org results in a 404" do
            result = delete(api_url("users/#{bad_user}"), platform.admin_user)
            result.should look_like({ :status=> 404,
                                      :body_exact => { "error" => target_not_found_in_org } })
        end
      end

      it "and can only response with 'accept' or 'reject'" do
        expected_msg = "Param response must be either 'accept' or 'reject'"
        invite_id = invite_user(platform.test_org.name, bad_user, platform.admin_user)
        user_invite_url = make_user_assoc_url_id(bad_user, invite_id)
        response = put(user_invite_url, platform.bad_user, :payload=>{:response=>"blither"})
        response.should look_like({ :status => 400,
                                    :body_exact => {"error"=> [expected_msg]}  })
        delete("#{org_assoc_url}/#{invite_id}", platform.admin_user).should look_like({ :status => 200 })
      end


      it "and get set up properly" do
        invite_id = invite_user(platform.test_org.name, bad_user, platform.admin_user)
        user_invite_url = make_user_assoc_url_id(bad_user, invite_id)
        response = put(user_invite_url, platform.bad_user, :payload=>{:response=>"accept"})
        response.should look_like({ :status => 200, :body => accept_response_body })

        # check that they are a member of the org
        user_should_be_in_org(platform.bad_user, platform.bad_user)
        # check that the admin user can see them
        user_should_be_in_org(platform.bad_user, platform.admin_user)
        user_should_be_in_group(platform.bad_user, 'users')

        delete(api_url("users/#{bad_user}"), platform.admin_user).should look_like({ :status=> 200 })
        user_should_not_be_in_org(platform.bad_user)
      end

      context "and a valid invite is issued" do
        let(:org) { platform.test_org.name }
        let(:invited_user_name) { "test-user-#{rand_id}-#{Process.pid}" }
        let(:invited_user) { platform.create_user(invited_user_name) }

        before :each do
          @invite_id = invite_user(org, invited_user.name, platform.admin_user)
          @user_invite_url = make_user_assoc_url_id(invited_user.name, @invite_id)
        end
        after :each do
          cleanup_requests_for_org(org)
          delete(api_url("users/#{invited_user_name}"), platform.superuser)
          delete("#{platform.server}/users/#{invited_user_name}", platform.superuser)
        end

        it "an org admin cannot accept that invite on behalf of the user", :authorization do
          response = put(@user_invite_url, platform.admin_user, :payload=>{:response=>"accept"})
          response.should look_like({ :status => 403 })
        end

        it "the global superuser can accept the invite on behalf of the user", :authorization do
          response = put(@user_invite_url, platform.superuser, :payload=>{:response=>"accept"})
          response.should look_like({ :status => 200})
        end
      end

      context "when the inviting admin" do
        let(:bad_user) { platform.bad_user.name }
        let(:org) { platform.test_org.name }
        let(:admin_username) { "test-admin-#{rand_id}-#{Process.pid}" }
        let(:test_admin_user) { platform.create_user(admin_username) }
        let(:test_username) { "test-user-#{rand_id}-#{Process.pid}" }
        let(:test_user) { platform.create_user(test_username) }
        let(:invalid_invite_msg) {
            "This invitation is no longer valid. Please notify an administrator and request to be re-invited to the organization."
        }

        before :each do
          platform.associate_user_with_org(org, test_admin_user)
          platform.add_user_to_group(org, test_admin_user, "admins")
          invite_id = invite_user(org, test_user.name, test_admin_user)
          @user_invite_url = make_user_assoc_url_id(test_user.name, invite_id)
        end

        after :each do
          # User may have already been disassociated and/or deleted depending on the test,
          # but clean up here just in case. Note use of 'superuser' - we're not trying to test permissions
          # just doing brute force cleanup.
          #
          cleanup_requests_for_org(org)
          platform.remove_user_from_group(org, test_admin_user, "admins")

          delete(api_url("users/#{test_user.name}"), platform.superuser)
          delete(api_url("users/#{test_admin_user.name}"), platform.superuser)

          delete("#{platform.server}/users/#{test_admin_user.name}", platform.superuser)
          delete("#{platform.server}/users/#{test_user.name}", platform.superuser)
        end

        it "is removed from admins group, invites issued by that admin cannot be accepted", :authorization do
            platform.remove_user_from_group(org, test_admin_user, "admins", platform.superuser)
            response = put(@user_invite_url, test_user, :payload=>{:response=>"accept"})
            response.should look_like({ :status => 403,
                                        :body_exact => { "error" => invalid_invite_msg } })
        end

        it "is removed from the org, invites issued by that admin cannot be accepted", :authorization do
          skip "Known failure: passes w/ 200 b/c no USAG cleanup performed for deleted user"

          delete(api_url("users/#{test_admin_username}"), platform.superuser)
          response = put(@user_invite_url, test_user, :payload=>{:response=>"accept"})
          response.should look_like({ :status => 403,
                                      :body_exact => { "error" => invalid_invite_msg } })
          no_invites_for_user(test_user)
        end
        it "is removed from the system, invites issued by that admin can't by accepted", :authorization do
          skip("Known failure: passes w/ 200 b/c no USAG or other group cleanup performed for deleted user")

          delete("#{platform.server}/users/#{test_admin_username}", platform.superuser)
          response = put(@user_invite_url, test_user, :payload=>{:response=>"accept"})
          response.should look_like({ :status => 403,
                                      :body_exact => { "error" => invalid_invite_msg } })
          no_invites_for_user(test_user)
        end
      end

      context "OC-11708 - when last updator of users group is dissociated" do
        let(:org) { platform.test_org.name }
        let(:admin_username) { "test-admin-#{rand_id}-#{Process.pid}" }
        let(:test_admin_user) { platform.create_user(admin_username) }
        let(:test_username) { "test-user-#{rand_id}-#{Process.pid}" }
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
        before(:each) do
          cleanup_requests_for_user(platform.bad_user)
          platform.create_org(other_org)
        end
        after(:each) do
          cleanup_requests_for_user(platform.bad_user)
          delete("#{platform.server}/organizations/#{other_org}/users/#{bad_user}", platform.superuser)
          platform.delete_org(other_org)
        end
        it "can accept an invite, even when they belong to another org" do

          platform.associate_user_with_org(other_org, platform.bad_user)

          invite_id_2 = invite_user(platform.test_org.name, platform.bad_user.name, platform.admin_user)
          accept_invite(platform.bad_user, platform.test_org.name, invite_id_2)

          no_invites_for_user(platform.bad_user)
          user_should_be_in_org(platform.bad_user)

          response = delete(api_url("users/#{bad_user}"), platform.admin_user)
          response.should look_like({ :status => 200 } )
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

      context "default client", :authorization do
        it "returns 403" do
          get(request_url, platform.non_admin_client).should look_like({
              :status => 403
            })
        end
      end

      context "outside user", :authorization do
        it "returns 403" do
          get(request_url, outside_user).should look_like({
              :status => 403
            })
        end
      end

      context "invalid user", :authentication do
        it "returns 401" do
          get(request_url, invalid_user).should look_like({
              :status => 401
            })
        end
      end
    end # context GET /organizations/<org>/users

    context "PUT /organizations/<org>/users" do
      context "admin user" do
        it "returns  405" do
          put(request_url, platform.admin_user).should look_like({
              :status => 405
            })
        end
      end
    end # context PUT /organizations/<org>/users

    context "DELETE /organizations/<org>/users" do
      context "admin user" do
        it "returns  405" do
          delete(request_url, platform.admin_user).should look_like({
              :status => 405
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
          "email" => "#{username}@chef.io",
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

      # !!TODO default normal user cannot get somebody else!
      #
      context "default normal user" do
        it "can get self", :smoke do
          get(request_url, platform.non_admin_user).should look_like({
              :status => 200,
              :body_exact => user_body
            })
        end
      end

      context "default client" do
        it "returns 403", :authorization do
          get(request_url, platform.non_admin_client).should look_like({
              :status => 403
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

      context "invalid user", :authentication do
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
        it "returns  405" do
          put(request_url, platform.admin_user).should look_like({
              :status => 405
            })
        end
      end
    end # context PUT /organizations/<org>/users/<name>

    context "POST /organizations/<org>/users/<name>" do
      it "as superuser returns 405" do
        post(request_url, platform.superuser).should look_like({
            :status => 405
          })
      end
      it "as org admin user returns 405" do
        post(request_url, platform.admin_user).should look_like({
            :status => 405
          })
      end
      it "as non-admin org user returns  405" do
        post(request_url, platform.non_admin_user).should look_like({
            :status => 405
          })
      end
    end # context POST /organizations/<org>/users/<name>

    context "DELETE /organizations/<org>/users/<name>" do
      let(:username) { "test-#{rand_id}-#{Process.pid}" }
      let(:test_user) { platform.create_user(username) }
      let(:org) { platform.test_org.name }

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
          before :each do
            platform.add_user_to_group(org, test_user, "admins")
          end
          after :each do
            platform.remove_user_from_group(org, test_user, "admins")
          end
          it "cannot delete own org association", :authorization do
            delete(request_url, test_user).should look_like({
                :status => 403,
                :body_exact => {"error" => "Please remove #{test_user.name} from this organization's admins group before removing him or her from the organization." }
              })
          end

          # Bug/regression check: prior to the fix, the user would be removed from the org member
          # list, even though other permissions were not modified.
          it "prevents deletion and does not break the admin org association in the process" do
            delete(request_url, test_user)
            # get users requires org membership to complete, so is a safe litmus to
            # verify that the user is still in the org
            get(api_url("users"), test_user).should look_like({ :status => 200 })
          end
        end
        it "when the actor is not an org admin, user can delete own association" do
          delete(request_url, test_user).should look_like({ :status => 200 })
        end
      end

      context "non-admin user", :authorization do
        it "returns 403" do
            delete(request_url, platform.non_admin_user).should look_like({ :status => 403 })
        end
      end

      context "default client", :authorization do
        it "returns 403" do
            delete(request_url, platform.non_admin_client).should look_like({ :status => 403 })
        end
      end

      context "when user doesn't exist" do
        let(:request_url) { api_url("users/bogus") }
        it "returns 404" do
          delete(request_url, platform.non_admin_user).should look_like({ :status => 404 })
          get(api_url("users"), platform.admin_user).should look_like({
              :status => 200,
              :body_exact => default_users_body + [ {"user" => {"username" => username}} ]})
        end
      end

      context "OC-11708 - when last updator of users group is dissociated" do
        let(:admin_username) { "test-admin-#{rand_id}-#{Process.pid}" }
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
