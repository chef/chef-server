describe "Groups Endpoint" do

  let(:requestor){superuser}

  context "/groups" do
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
    context "POST"

    should_not_allow :PUT, "/groups"
    should_not_allow :DELETE, "/groups"
  end # /groups

  context "/groups/<group_id>" do
    # GET returns the actors and groups in the group
    #
    # Note that only DIRECT membership is reflected in group
    # membership lists (i.e., if an actor X is a member of group A,
    # which itself is a member of group B, then X will not appear in
    # the actors list of group B, even though X behaves as though it
    # does)
    #
    # Can't get a group if the requesting actor isn't in the READ ACE
    context "GET"

    should_not_allow :POST, "/groups/ffffffffffffffffffffffffffffffff"
    should_not_allow :PUT, "/groups/ffffffffffffffffffffffffffffffff"

    # DELETE deletes the group and its ACL and ACEs
    #
    # Cucumber: Deleting actor must be within the DELETE ACE (directly or indirectly)
    #
    # Cucumber: Deleting a non-existent group should return 404
    context "DELETE"
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
    context "PUT"

    # Delete an actor / group from the group
    #
    # Members of actors and group lists can be removed.  Removing
    # members that aren't actually members fails with 404.  Ditto for
    # removing "members" that don't really exist.
    #
    # Can't delete members without the UPDATE ACE
    context "DELETE"
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
    context "GET"

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
    context "GET"

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
    context "PUT"

    # DELETE clears actors and groups from ACE
    context "DELETE"
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
    context "GET"

    should_not_allow :POST, "/groups/ffffffffffffffffffffffffffffffff/acl/create/actors/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"
    should_not_allow :PUT, "/groups/ffffffffffffffffffffffffffffffff/acl/create/actors/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"
    should_not_allow :DELETE, "/groups/ffffffffffffffffffffffffffffffff/acl/create/actors/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"
  end # /groups/<group_id>/acl/<action>/<member_type>/<member_id>

end
