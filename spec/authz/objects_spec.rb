describe "Objects Endpoint" do
  let(:mattdamon) { "deadbeefdeadbeefdeadbeefdeadbeef" }
  let(:toupee) { "deadbeefdeadbeefdeadbeefdeadbeef" }
  let(:god) { "deadbeefdeadbeefdeadbeefdeadbeef" }

  context "/objects" do
    # What we are testing:

    # Here we test object creation (all other HTTP verbs should be
    # disallowed), making sure the response body is correct and that
    # id and id in the uri match, as well as basic header validation,
    # as well as making sure that the requesting actor is contained in
    # the newly created object's ACLs.

    should_not_allow :GET, "/objects"

    # POST creates a new object and its ACL, creating and
    # pre-populating its ACEs with the requesting actor
    #
    # If the superuser creates an object, though, the ACL
    # should be empty.

    context "POST" do

      # We mainly do this to make sure the test cleans up after
      # itself; otherwise we have to repeat the hacky after :each with
      # the @object_id stuff, and, well this is pretty much the same
      # for every creation
      def self.creates_object_as(requestor, headers = {})
        after :each do
          delete("/objects/#{@object_id}", :superuser)
        end

        it "creates an object" do
          response = post("/objects", requestor, headers)

          # TODO: de-hardcode uri hostname in response body, make configurable
          response.should have_status_code(201).
            with_body({"id" => /^[0-9a-f]{32}$/,
                       "uri" => /^http\:\/\/authz\.opscode\.com\/objects\/[0-9a-f]{32}$/})
        
          @object_id = parse(response)["id"]

          # Verify that uri and id are the same
          uri_id = parse(response)["uri"].split("/")[-1]
          uri_id.should == @object_id
        end
      end

      context "as a superuser" do
        creates_object_as(:superuser)
      end

      # Should this work?
      context "as an unknown requestor" do
        let(:fake_actor) { mattdamon }

        creates_object_as(:fake_actor)
      end

      context "without the X-Ops-Requesting-Actor-Id header" do
        it "should not create an object" do
          response = post("/objects", :superuser,
                          :merge_headers => {"X-Ops-Requesting-Actor-Id" => :DELETE})

          response.should have_status_code(403).
            with_body({"error" => "must specify a requesting actor id"})
        end
      end

      # Not quite clear the purpose of this header, actually
      context "without the X-Ops-User-Id header" do
        creates_object_as(:superuser,
                         :merge_headers => {"X-Ops-User-Id" => :DELETE})
      end

      context "without ANY of the standard headers except Content-Type" do
        it "should not create an object" do
          response = post("/objects", :superuser,
                          :headers => {"Content-Type" => "application/json"})

          response.should have_status_code(403).
            with_body({"error" => "must specify a requesting actor id"})
        end
      end

      context "without any headers" do
        it "should not create an object" do
          post("/objects", :superuser, :headers => {}).should have_status_code(403).
            with_body({"error" => "must specify a requesting actor id"})
        end
      end

      context "created object" do
        with_actor :shatner

        before :each do
          response = post("/objects", shatner)
          @object = parse(response)["id"]
        end

        after :each do
          delete("/objects/#{@object}", shatner)
        end

        it "contains creator in ACLs" do
          body = {"create" => {"actors" => [shatner], "groups" => []},
            "read" => {"actors" => [shatner], "groups" => []},
            "update" => {"actors" => [shatner], "groups" => []},
            "delete" => {"actors" => [shatner], "groups" => []},
            "grant" => {"actors" => [shatner], "groups" => []}}
          
          get("/objects/#{@object}/acl",
              :superuser).should have_status_code(200).with_body(body)
        end
      end
    end # POST

    should_not_allow :PUT, "/objects"
    should_not_allow :DELETE, "/objects"
  end # /objects

  context "/objects/<object_id>" do
    # What we are testing:

    # Here we test object existence with GET (should require
    # appropriate READ access), as well as the ability to delete
    # objects (should require appropriate DELETE access).  All other
    # HTTP verbs should be disallowed.
    context "GET" do
      context "an actor directly in the READ ACE" do
        with_actor :hasselhoff
        with_object :spork

        with_ace_on :spork, :read, :actors => [:hasselhoff]

        it "can read the object" do
          get("/objects/#{spork}",
              :hasselhoff).should have_status_code(200).with_body({})
        end
      end

      context "an actor NOT in the READ ACE" do
        with_actor :malkovich
        with_object :spork

        # Give malkovich everything EXCEPT read
        with_acl_on :spork, {
          :create => {:actors => [:malkovich], :groups => []},
          :read   => {:actors => [],           :groups => []}, # <--- That's the one!
          :update => {:actors => [:malkovich], :groups => []},
          :delete => {:actors => [:malkovich], :groups => []},
          :grant  => {:actors => [:malkovich], :groups => []}
        }

        it "cannot read the object" do
          get("/objects/#{spork}", :malkovich).should have_status_code(403).
            with_body({"error" => "must be in the read access control entry to perform this action"})
        end
      end

      context "an actor indirectly in the READ ACE" do
        with_actor :hasselhoff
        with_group :hipsters, :actors => [:hasselhoff]
        with_object :spork

        with_ace_on :spork, :read, :groups => [:hipsters]

        it "can read the object" do
          get("/objects/#{spork}",
              :hasselhoff).should have_status_code(200).with_body({})
        end
      end

      context "with a non-existent target" do
        with_actor :hasselhoff

        it "can't be read, because it doesn't exist" do
          fake_object = toupee

          get("/objects/#{fake_object}", :hasselhoff).should have_status_code(404)
        end
      end
    end # GET

    should_not_allow :POST, "/objects/ffffffffffffffffffffffffffffffff"
    should_not_allow :PUT, "/objects/ffffffffffffffffffffffffffffffff"

    # DELETE deletes the object and its ACL and ACEs
    context "DELETE" do
      context "an actor directly in the DELETE ACE" do
        with_actor :hasselhoff
        with_object :spork

        with_ace_on :spork, :delete, :actors => [:hasselhoff]

        it "can delete the object" do
          delete("/objects/#{spork}",
                 :hasselhoff).should have_status_code(200).with_body({})
          get("/objects/#{spork}", :superuser).should have_status_code(404)
        end
      end

      context "an actor NOT in the DELETE ACE" do
        with_actor :malkovich
        with_object :spork

        # Give malkovich everything EXCEPT delete
        with_acl_on :spork, {
          :create => {:actors => [:malkovich], :groups => []},
          :read   => {:actors => [:malkovich], :groups => []},
          :update => {:actors => [:malkovich], :groups => []},
          :delete => {:actors => [],           :groups => []}, # <--- That's the one!
          :grant  => {:actors => [:malkovich], :groups => []}
        }

        it "cannot delete the object" do
          delete("/objects/#{spork}", :malkovich).should have_status_code(403).
            with_body({"error" => "must be in the delete access control entry to perform this action"})
          get("/objects/#{spork}", :superuser).should have_status_code(200)
        end
      end

      context "an actor indirectly in the DELETE ACE" do
        with_actor :hasselhoff
        with_group :hipsters, :actors => [:hasselhoff]
        with_object :spork

        with_ace_on :spork, :delete, :groups => [:hipsters]

        it "can delete the object" do
          delete("/objects/#{spork}",
                 :hasselhoff).should have_status_code(200).with_body({})
          get("/objects/#{spork}", :superuser).should have_status_code(404)
        end
      end

      context "with a non-existent target" do
        with_actor :hasselhoff

        it "can't be deleted, because it doesn't exist" do
          fake_object = toupee

          # Prove it doesn't exist
          get("/objects/#{fake_object}", :hasselhoff).should have_status_code(404)

          # Now try to delete it
          delete("/objects/#{fake_object}", :hasselhoff).should have_status_code(404)
        end
      end
    end # GET
  end # /objects/<object_id>

  context "/<type>/<id>/acl" do
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
    # What we are testing:

    # Here we test access to a specific ACE/action in each type's ACL
    # and that the response body has the correct format.  Apparently,
    # any ACE at all grants access to the ACL -- we test each ACE in
    # turn, both directly and indirectly through a group.  PUT is used
    # for updating the ACL and is likewise tested (although there is
    # currently no checking for request correctness, and authz will
    # crash on badly formatted requests).  DELETE is also tested,
    # however it seems to be broken.  HTTP POST should be disallowed.

    # Old notes:

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
    
    ['group', 'container', 'object'].each do |type|
      context "for #{type.upcase} type" do
        context "GET" do
          ['create', 'read', 'update', 'delete', 'grant'].each do |action|
            context "for #{action.upcase} action" do
              ['create', 'read', 'update', 'delete', 'grant'].each do |ace|

                context "an actor directly in the #{ace.upcase} ACE" do
                  with_actor :hasselhoff
                  with_entity type.to_sym, :gozer

                  with_ace_on :gozer, ace.to_sym, :actors => [:hasselhoff]

                  if (action == ace)
                    let(:body) { {"actors" => [hasselhoff], "groups" => []} }
                  else
                    let(:body) { {"actors" => [], "groups" => []} }
                  end

                  it "can read the acl" do
                    get("/#{type}s/#{gozer}/acl/#{action}",
                        :hasselhoff).should have_status_code(200).with_body(body)
                  end
                end

                context "an actor indirectly in the #{ace.upcase} ACE" do
                  with_actor :hasselhoff
                  with_group :hipsters, :actors => [:hasselhoff]
                  with_entity type.to_sym, :gozer

                  with_ace_on :gozer, ace.to_sym, :groups => [:hipsters]

                  if (action == ace)
                    let(:body) { {"actors" => [], "groups" => [hipsters]} }
                  else
                    let(:body) { {"actors" => [], "groups" => []} }
                  end

                  it "can read the acl" do
                    get("/#{type}s/#{gozer}/acl/#{action}",
                        :hasselhoff).should have_status_code(200).with_body(body)
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
                    get("/#{type}s/#{gozer}/acl/#{action}",
                        :malkovich).should have_status_code(403).with_body({"error" => "must be in one of the create, read, update, delete, grant access control entries to perform this action"})
                  end
                end

                context "with a non-existent target" do
                  with_actor :hasselhoff

                  it "can't be read, because it doesn't exist" do
                    fake_entity = god

                    get("/#{type}s/#{fake_entity}/acl/#{action}",
                        :hasselhoff).should have_status_code(404)
                  end
                end
              end
            end
          end
        end # GET

        ['create', 'read', 'update', 'delete', 'grant'].each do |action|
          context "for #{action.upcase} action" do
            should_not_allow :POST, "/#{type}s/ffffffffffffffffffffffffffffffff/acl/#{action}"
          end
        end

        # PUT replaces an ACE atomically
        context "PUT", :focus do
        end # PUT

        # Only actors in the DELETE ACE (directly or indirectly) can delete an object
        #
        # Deleting a non-existent object returns a 404
        #
        context "DELETE" do
        end # DELETE
      end
    end
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
