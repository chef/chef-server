describe "Containers Endpoint" do

  let(:requestor){superuser}

  context "/containers" do
    should_not_allow :GET, "/containers"

    # POST creates a new container and its ACL, creating and pre-populating its ACEs with
    # the requesting actor
    #
    # NOTE: the return value for this a {"id": ID, "uri": URI} hash
    #
    # Creating a container is different than creating other items,
    # because containers actually have real names (not hex IDs).  As a
    # result, you have to POST with a JSON body.
    #
    # Can't quite tell what the JSON looks like (the Cuke tests
    # obfuscate that a bit), but I assume it's something like {"name":
    # "my_container"} (though maybe it's just "my_container"?)
    #
    # Cucumber: creating a container without a requesting actor header
    # should fail with 403
    context "POST"

    should_not_allow :PUT, "/containers"
    should_not_allow :DELETE, "/containers"
  end # /containers

  context "/containers/<container_id>" do
    # GET returns the container
    #
    # NOTE: This is borderline pointless, since it always returns an
    # empty JSON object!  It can basically only answer the question
    # "Is there a container with this ID?"
    context "GET"

    should_not_allow :POST, "/containers/fake"
    should_not_allow :PUT, "/containers/fake"

    # DELETE deletes the container
    #
    # NOTE: Do we ever actually delete containers?
    #
    # According to comments for oc_chef_authz:delete_resource/3, we do
    # not, so we may very well be able to disallow DELETE as well.
    #
    # Cucumber: an actor must be in the DELETE ACE of a container
    # (directly or indirectly) to delete it.
    #
    # Cucumber: cannot delete a container if not in the DELETE ACE
    #
    # Cucumber: deleting a non-existent container returns a 404
    context "DELETE"
  end # /containers/<container_id>

  context "/containers/<container_id>/acl" do
    # GET full ACL if the requesting actor (from the header) has grant
    # permission on the container
    context "GET"

    # NOTE: We'll want to eventually allow these operations in order
    # to facilitate bulk operations
    #
    # (Though with containers, I'm not sure that it's worth it, since
    # I think fixed containers are basically created one time at
    # database initialization... would need to verify that, though)
    should_not_allow :POST, "/containers/fake/acl"
    should_not_allow :PUT, "/containers/fake/acl"
    should_not_allow :DELETE, "/containers/fake/acl"
  end # /containers/<container_id>/acl

  context "/containers/<container_id>/acl/<action>" do
    # GET actors and groups for action
    context "GET"

    should_not_allow :POST, "/containers/fake/acl/create"

    # PUT replaces an ACE atomically
    context "PUT"

    # DELETE clears actors and groups from ACE
    context "DELETE"
  end # /containers/<container_id>/acl/<action>

  context "/containers/<container_id>/acl/<action>/<member_type>/<member_id>" do
    # GET uses is_authorized_on_object to determine whether the
    # specified actor / group has the specified permission
    #
    # Returns a 200 and an empty JSON hash if the actor / group has
    # the permission
    #
    # TODO: Perhaps use 204 (OK, No Content) instead?
    context "GET"

    should_not_allow :POST, "/containers/fake/acl/create/actors/ffffffffffffffffffffffffffffffff"
    should_not_allow :PUT, "/containers/fake/acl/create/actors/ffffffffffffffffffffffffffffffff"
    should_not_allow :DELETE, "/containers/fake/acl/create/actors/ffffffffffffffffffffffffffffffff"
  end # /containers/<container_id>/acl/<action>/<member_type>/<member_id>
end
