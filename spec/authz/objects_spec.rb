describe "Objects Endpoint" do

  # Superusers should be able to read any ACL, update any ACL, delete
  # objects, etc.  This applies everywhere.
  let(:requestor){superuser}

  # Create a new object
  context "/objects" do
    should_not_allow :GET, "/objects"

    # POST creates a new object and its ACL, creating and
    # pre-populating its ACEs with the requesting actor
    #
    # If the superuser creates an object, though, the ACL
    # should be empty.
    context "POST" do
      context "without the X-Ops-Requesting-Actor-Id header" do
        it "fails" do
          post("/objects",
               :superuser,
               :merge_headers => {"X-Ops-Requesting-Actor-Id" => :DELETE}
               ).should have_status_code(403).with_body({"error" => "must specify a requesting actor id"})
        end
      end
    end

    should_not_allow :PUT, "/objects"
    should_not_allow :DELETE, "/objects"
  end # /objects

  # Manipulate a specific object
  context "/objects/<object_id>" do
    # NOTE: This appears to be essentially a meaningless operation
    # right now... looks like it always returns an empty JSON hash
    context "GET"

    should_not_allow :POST, "/objects/ffffffffffffffffffffffffffffffff"
    should_not_allow :PUT, "/objects/ffffffffffffffffffffffffffffffff"

    # DELETE deletes the object and its ACL and ACEs
    context "DELETE"
  end # /objects/<object_id>

  # See all permissions on a given object
  context "/objects/<object_id>/acl" do
    # GET full ACL if the requesting actor (from the header) has grant
    # permission on the object or group
    #
    # Cucumber: tests each ACE individually to ensure the requesting
    # actor is present... not sure if it also compares that to the
    # full ACL to make sure they're the same.
    context "GET"

    # NOTE: We'll want to eventually allow these operations in order
    # to facilitate bulk operations
    should_not_allow :POST, "/objects/ffffffffffffffffffffffffffffffff/acl"
    should_not_allow :PUT, "/objects/ffffffffffffffffffffffffffffffff/acl"
    should_not_allow :DELETE, "/objects/ffffffffffffffffffffffffffffffff/acl"
  end # /objects/<object_id>/acl

  # Manipulate a specific permission on a given object
  context "/objects/<object_id>/acl/<action>" do
    # GET actors and groups for action
    #
    # Cucumber: ensure a newly-created object has the requesting actor
    # in the ACEs for each permission
    #
    # Cucumber: actors that are directly or indirectly (i.e., via a
    # group) in an ACE are recognized as having that permission.
    #
    # An actor (that exists!) that is not in any ACE has no permissions.
    #
    # An actor (that doesn't exist!) has no permissions anyway.
    #
    # (Note that the above two scenarios both result in HTTP 404s)
    #
    # Also, it appears that we don't check to see if an ID is actually
    # a valid 32-character hex string, since the test uses
    # non-existent IDs like "frankenberry" (presumably this applies
    # elsewhere, and not just for this endpoint.)
    #
    # Actually... this may be just an artifact of how the Cuke tests
    # are written.
    context "GET"

    should_not_allow :POST, "/objects/ffffffffffffffffffffffffffffffff/acl/create"

    # PUT replaces an ACE atomically
    context "PUT"

    # Only actors in the DELETE ACE (directly or indirectly) can delete an object
    #
    # Deleting a non-existent object returns a 404
    #
    context "DELETE"
  end # /objects/<object_id>/acl/<action>

  # Query the permission granted on an object of a given actor or group
  context "/objects/<object_id>/acl/<action>/<member_type>/<member_id>" do
    # GET uses is_authorized_on_object to determine whether the
    # specified actor / group has the specified permission
    #
    # Returns a 200 and an empty JSON hash if the actor / group has
    # the permission
    #
    # TODO: Perhaps use 204 (OK, No Content) instead?
    #
    # Cucumber: If object_id refers to a non-existent object, a 404
    # should be returned (there is an open ticket for this, PL-536;
    # this used to (still does?) cause a crash)
    #
    # Deleting one group from a given ACE shouldn't affect other
    # holders of that permission that were not in that group
    #
    # (however if an actor is in multiple groups, and one of them is
    # removed, that actor should still have the permission by virtue
    # of their other group membership!  This wasn't tested in Cuke.)
    context "GET"

    should_not_allow :POST, "/objects/ffffffffffffffffffffffffffffffff/acl/create/actors/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"
    should_not_allow :PUT, "/objects/ffffffffffffffffffffffffffffffff/acl/create/actors/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"
    should_not_allow :DELETE, "/objects/ffffffffffffffffffffffffffffffff/acl/create/actors/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"
  end # /objects/<object_id>/acl/<action>/<member_type>/<member_id>
end
