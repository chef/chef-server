describe "Objects Endpoint" do
  let(:mattdamon) { "deadbeefdeadbeefdeadbeefdeadbeef" }
  let(:god) { "deadbeefdeadbeefdeadbeefdeadbeef" }

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

  context "/objects/<object_id>" do
    # NOTE: This appears to be essentially a meaningless operation
    # right now... looks like it always returns an empty JSON hash
    context "GET"

    should_not_allow :POST, "/objects/ffffffffffffffffffffffffffffffff"
    should_not_allow :PUT, "/objects/ffffffffffffffffffffffffffffffff"

    # DELETE deletes the object and its ACL and ACEs
    context "DELETE"
  end # /objects/<object_id>

  context "/<type>/<id>/acl", :focus do
    # What we are testing:

    # Here we test access to type's ACL and that the response body
    # has the correct format.  Apparently, any ACE at all grants
    # access to the ACL (is this a good idea?[1]) -- we test each ACE in
    # turn, both directly and indirectly through a group.  All other
    # HTTP verbs should be disallowed.

    # [1] Apparently done intentionally, but the reasons are lost to the
    # mists of time.  Would be nice to find those reasons again.
    ['group', 'container', 'object'].each do |type|
      context "for #{type.upcase} type" do
        context "GET" do
          ['create', 'read', 'update', 'delete', 'grant'].each do |ace|

            context "an actor directly in the #{ace.upcase} ACE" do
              with_actor :hasselhoff
              with_entity type.to_sym, :gozer

              with_ace_on :gozer, ace.to_sym, :actors => [:hasselhoff]

              it "can read the acl" do
                body = {
                  "create" => {"actors" => [], "groups" => []},
                  "read" => {"actors" => [], "groups" => []},
                  "update" => {"actors" => [], "groups" => []},
                  "delete" => {"actors" => [], "groups" => []},
                  "grant" => {"actors" => [], "groups" => []}
                }
                body[ace] = {"actors" => [hasselhoff], "groups" => []}

                get("/#{type}s/#{gozer}/acl",
                    :hasselhoff).should have_status_code(200).with_body(body)
              end
            end

            context "an actor indirectly in the #{ace.upcase} ACE" do
              with_actor :hasselhoff
              with_group :hipsters, :actors => [:hasselhoff]
              with_entity type.to_sym, :gozer

              with_ace_on :gozer, ace.to_sym, :groups => [:hipsters]

              it "can read the acl" do
                body = {
                  "create" => {"actors" => [], "groups" => []},
                  "read" => {"actors" => [], "groups" => []},
                  "update" => {"actors" => [], "groups" => []},
                  "delete" => {"actors" => [], "groups" => []},
                  "grant" => {"actors" => [], "groups" => []}
                }
                body[ace] = {"actors" => [], "groups" => [hipsters]}

                get("/#{type}s/#{gozer}/acl",
                    :hasselhoff).should have_status_code(200).with_body(body)
              end
            end
          end

          context "an actor with NO ACE" do
            with_actor :malkovich
            with_entity type.to_sym, :gozer

            # Give malkovich no access at all
            with_acl_on :gozer, {
              :create => {:actors => [], :groups => []},
              :read   => {:actors => [], :groups => []},
              :update => {:actors => [], :groups => []},
              :delete => {:actors => [], :groups => []},
              :grant  => {:actors => [], :groups => []}
            }

            it "cannot read the acl" do
              get("/#{type}s/#{gozer}/acl", :malkovich).should have_status_code(403).
                with_body({"error" => "must be in one of the create, read, update, delete, grant access control entries to perform this action"})
            end
          end

          context "with a non-existent target" do
            with_actor :hasselhoff

            it "can't be read, because it doesn't exist" do
              fake_entity = god

              get("/#{type}s/#{fake_entity}/acl",
                  :hasselhoff).should have_status_code(404)
            end
          end
        end # GET

        # NOTE: We'll want to eventually allow these operations in order
        # to facilitate bulk operations
        should_not_allow :POST, "/#{type}s/ffffffffffffffffffffffffffffffff/acl"
        should_not_allow :PUT, "/#{type}s/ffffffffffffffffffffffffffffffff/acl"
        should_not_allow :DELETE, "/#{type}s/ffffffffffffffffffffffffffffffff/acl"
      end
    end
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
