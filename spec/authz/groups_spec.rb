describe "Groups Endpoint" do
  let(:mattdamon) { "deadbeefdeadbeefdeadbeefdeadbeef" }
  let(:honest_politicians) { "deadbeefdeadbeefdeadbeefdeadbeef" }

  context "/groups" do
    # What we are testing:

    # Here we test group creation (all other HTTP verbs should be
    # disallowed), making sure the response body is correct and that
    # id and id in the uri match, as well as basic header validation,
    # as well as making sure that the requesting actor is contained in
    # the newly created groups ACLs.

    should_not_allow :GET, "/groups"

    # POST creates a new group and its ACL, creating and pre-populating
    # its ACEs with the requesting actor
    #
    # Returns a standard id/uri JSON object
    #
    # No body is accepted; currently groups are not created with any
    # members (this would be a good feature to add for speeding up
    # Authz).
    #
    # Cucumber: creating a group without a requesting actor should fail with 403
    context "POST" do

      # We mainly do this to make sure the test cleans up after
      # itself; otherwise we have to repeat the hacky after :each with
      # the @group_id stuff, and, well this is pretty much the same
      # for every creation
      def self.creates_group_as(requestor, headers = {})
        after :each do
          delete("/groups/#{@group_id}", :superuser)
        end

        it "creates a group" do
          response = post("/groups", requestor, headers)

          # TODO: de-hardcode uri hostname in response body, make configurable
          response.should have_status_code(201).
            with_body({"id" => /^[0-9a-f]{32}$/,
                        "uri" => /^http\:\/\/authz\.opscode\.com\/groups\/[0-9a-f]{32}$/})
        
          @group_id = parse(response)["id"]

          # Verify that uri and id are the same
          uri_id = parse(response)["uri"].split("/")[-1]
          uri_id.should == @group_id
        end
      end

      context "as a superuser" do
        creates_group_as(:superuser)
      end

      # Should this work?
      context "as an unknown requestor" do
        let(:fake_actor) { mattdamon }

        creates_group_as(:fake_actor)
      end

      # Apparently, this is the only item creation operation that
      # doesn't require this header; is this the "correct" behavior?
      context "without the X-Ops-Requesting-Actor-Id header" do
        it "should not create a group" do
          response = post("/groups", :superuser,
                          :merge_headers => {"X-Ops-Requesting-Actor-Id" => :DELETE})

          response.should have_status_code(403).
            with_body({"error" => "must specify a requesting actor id"})
        end
      end

      # Not quite clear the purpose of this header, actually
      context "without the X-Ops-User-Id header" do
        creates_group_as(:superuser,
                         :merge_headers => {"X-Ops-User-Id" => :DELETE})
      end

      context "without ANY of the standard headers except Content-Type" do
        it "should not create a group" do
          response = post("/groups", :superuser,
                          :headers => {"Content-Type" => "application/json"})

          response.should have_status_code(403).
            with_body({"error" => "must specify a requesting actor id"})
        end
      end

      context "without any headers" do
        it "should not create a group" do
          post("/groups", :superuser, :headers => {}).should have_status_code(403).
            with_body({"error" => "must specify a requesting actor id"})
        end
      end

      context "created group" do
        with_actor :shatner

        before :each do
          response = post("/groups", shatner)
          @group = parse(response)["id"]
        end

        after :each do
          delete("/groups/#{@group}", shatner)
        end

        it "contains creator in ACLs" do
          body = {"create" => {"actors" => [shatner], "groups" => []},
            "read" => {"actors" => [shatner], "groups" => []},
            "update" => {"actors" => [shatner], "groups" => []},
            "delete" => {"actors" => [shatner], "groups" => []},
            "grant" => {"actors" => [shatner], "groups" => []}}
          
          get("/groups/#{@group}/acl",
              :superuser).should have_status_code(200).with_body(body)
        end
      end
    end # POST

    should_not_allow :PUT, "/groups"
    should_not_allow :DELETE, "/groups"
  end # /groups

  context "/groups/<group_id>", :focus do
    # GET returns the actors and groups in the group
    #
    # Note that only DIRECT membership is reflected in group
    # membership lists (i.e., if an actor X is a member of group A,
    # which itself is a member of group B, then X will not appear in
    # the actors list of group B, even though X behaves as though it
    # does)
    #
    # Can't get a group if the requesting actor isn't in the READ ACE
    context "GET" do
      context "an actor directly in the READ ACE" do
        with_actor :hasselhoff
        with_group :hipsters

        with_ace_on_group :hipsters, :read, :actors => [:hasselhoff]

        it "can read the group" do
          get("/groups/#{hipsters}",
              :hasselhoff).should have_status_code(200).with_body({"actors" => [],
                                                                    "groups" => []})
        end
      end

      context "an actor NOT in the READ ACE" do
        with_actor :malkovich
        with_group :hipsters

        # Give malkovich everything EXCEPT read
        with_acl_on_group :hipsters, {
          :create => {:actors => [:malkovich], :groups => []},
          :read   => {:actors => [],           :groups => []}, # <--- That's the one!
          :update => {:actors => [:malkovich], :groups => []},
          :delete => {:actors => [:malkovich], :groups => []},
          :grant  => {:actors => [:malkovich], :groups => []}
        }

        it "cannot read the group" do
          get("/groups/#{hipsters}", :malkovich).should have_status_code(403).
            with_body({"error" => "must be in the read access control entry to perform this action"})
        end
      end

      context "an actor indirectly in the READ ACE" do
        with_actor :hasselhoff
        with_groups :hipsters, :brogrammers

        with_ace_on_group :brogrammers, :read, :groups => [:hipsters]
        with_members :hipsters, :actors => [:hasselhoff]

        it "can read the groups" do
          get("/groups/#{brogrammers}",
              :hasselhoff).should have_status_code(200).with_body({"actors" => [],
                                                                    "groups" => []})
        end
      end

      context "with a non-existent target" do
        with_actor :hasselhoff

        it "can't be read, because it doesn't exist" do
          fake_group = honest_politicians

          get("/groups/#{fake_group}", :hasselhoff).should have_status_code(404)
        end
      end
    end # GET

    should_not_allow :POST, "/groups/ffffffffffffffffffffffffffffffff"
    should_not_allow :PUT, "/groups/ffffffffffffffffffffffffffffffff"

    # DELETE deletes the group and its ACL and ACEs
    #
    # Cucumber: Deleting actor must be within the DELETE ACE (directly or indirectly)
    #
    # Cucumber: Deleting a non-existent group should return 404
    context "DELETE" do
    end # DELETE
  end # /groups/<group_id>

  # Note: In the below tests, using a non-existent group_id should return 404, NOT 500!
  # See https://tickets.corp.opscode.com/browse/PL-536

  # Alter group membership
  context "/groups/<group_id>/<member_type>/<member_id>" do
    should_not_allow :GET, "/groups/ffffffffffffffffffffffffffffffff/actors/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"
    should_not_allow :POST, "/groups/ffffffffffffffffffffffffffffffff/actors/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"

    # Add an actor / group to the group
    #
    # Actors go in the actor group, Groups go in the groups group
    # (BEHOLD THE AWESOME POWER OF THE ENGLISH LANGUAGE!).  Actors and
    # groups must exist.
    #
    # Note: I don't think that there's any body to this request... it
    # looks like you just have to do a PUT to
    # /groups/<mygroup>/actors/<actor_I_want_in_the_group> and it
    # happens.  Unsure what happens if the actor is already in the
    # group (should be a no-op, I would imagine)
    #
    # Multiple actors and groups can be in a group
    #
    # A GROUP CAN ACTUALLY BE A MEMBER OF ITSELF?!?!
    #
    # CYCLES OF GROUP MEMBERSHIP CAN ACTUALLY BE CREATED! (e.g. g1 <-
    # g2 <- g3 <- g1)
    #
    # The effects of these recursive structures is apparently
    # "normal", but I think the structure is preserved, which is
    # potentially confusing.
    #
    # Can't add members without the UPDATE ACE
    #
    # Can't add non-existent actors or groups to a group (but it returns a 403?)
    #
    # Trying to add an actor to the groups list should fail, and vice versa.
    context "PUT" do
    end # PUT

    # Delete an actor / group from the group
    #
    # Members of actors and group lists can be removed.  Removing
    # members that aren't actually members fails with 404.  Ditto for
    # removing "members" that don't really exist.
    #
    # Can't delete members without the UPDATE ACE
    context "DELETE" do
    end # DELETE
  end

  # See all permissions on a given group
  context "/groups/<group_id>/acl" do
    # GET full ACL if the requesting actor (from the header) has grant
    # permission on the group
    #
    # Cucumber: an actor that is indirectly in the GRANT ACE of a
    # group can GET the ACL
    #
    # That is, if "Group Foo" is in the GRANT ACE of "Group Bar", and
    # Bob is in "Group Foo", then he's recognized as having GRANT
    # permissions
    #
    # This seems to imply that non-GRANT actors can't access a group's
    # ACL?
    #
    # Another Cucumber test *says* this is the case, but the steps
    # being run directly contradict that.  Unsure if this is a problem
    # with the tests or the description :(
    context "GET" do
    end # GET

    # NOTE: We'll want to eventually allow these operations in order
    # to facilitate bulk operations
    should_not_allow :POST, "/groups/ffffffffffffffffffffffffffffffff/acl"
    should_not_allow :PUT, "/groups/ffffffffffffffffffffffffffffffff/acl"
    should_not_allow :DELETE, "/groups/ffffffffffffffffffffffffffffffff/acl"
  end # /groups/<group_id>/acl

  # Manipulate a specific permission on a given group
  context "/groups/<group_id>/acl/<action>" do
    # GET actors and groups for action
    #
    # Cucumber: the group's creator (i.e., the one with GRANT
    # privileges) can read every ACE
    context "GET" do
    end # GET

    should_not_allow :POST, "/groups/ffffffffffffffffffffffffffffffff/acl/create"

    # PUT replaces an ACE atomically
    #
    # Cucumber: tests that a requestor can add/remove both users and groups
    # to each ACE, provided they (the requestor) created the group
    # (i.e., have the GRANT permission)
    #
    # BUG: apparently, we can add non-existent objects to ACLs!  The
    # cucumber tests reference PL-537.
    #
    # I encountered the test in this context, but presumably it
    # applies to all endpoints.
    #
    # Cucumber: non-GRANTors cannot update ACEs at all
    context "PUT" do
    end # GET

    # DELETE clears actors and groups from ACE
    context "DELETE" do
    end # GET
  end # /groups/<group_id>/acl/<action>

  # Query the permission granted on an group of a given actor or group
  context "/groups/<group_id>/acl/<action>/<member_type>/<member_id>" do
    # GET uses is_authorized_on_object to determine whether the
    # specified actor / group has the specified permission
    #
    # Returns a 200 and an empty JSON hash if the actor / group has
    # the permission
    #
    # TODO: Perhaps use 204 (OK, No Content) instead?
    context "GET" do
    end # GET

    should_not_allow :POST, "/groups/ffffffffffffffffffffffffffffffff/acl/create/actors/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"
    should_not_allow :PUT, "/groups/ffffffffffffffffffffffffffffffff/acl/create/actors/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"
    should_not_allow :DELETE, "/groups/ffffffffffffffffffffffffffffffff/acl/create/actors/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"
  end # /groups/<group_id>/acl/<action>/<member_type>/<member_id>

end
