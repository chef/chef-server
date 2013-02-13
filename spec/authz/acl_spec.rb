describe "ACL tests", :focus do
  let(:easterbunny) { "deadbeefdeadbeefdeadbeefdeadbeef" }

  context "/<type>/<id>/acl" do
    # What we are testing:

    # Here we test access to type's ACL and that the response body
    # has the correct format.  Apparently, any ACE at all grants
    # access to the ACL (is this a good idea?[1]) -- we test each ACE in
    # turn, both directly and indirectly through a group.  All other
    # HTTP verbs should be disallowed.

    # [1] Apparently done intentionally, but the reasons are lost to the
    # mists of time.  Would be nice to find those reasons again.
    [:group, :container, :object].each do |type|
      context "for #{type.upcase} type" do
        context "GET" do
          ['create', 'read', 'update', 'delete', 'grant'].each do |ace|

            context "an actor directly in the #{ace.upcase} ACE" do
              with_actor :hasselhoff
              with_entity type, :gozer

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
              with_entity type, :gozer

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
            with_entity type, :gozer

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
              fake_entity = easterbunny

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
  end # /<type>/<id>/acl

  # Manipulate a specific permission on a given object
  context "/<type>/<id>/acl/<action>" do
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
    
    [:group, :container, :object].each do |type|
      context "for #{type.upcase} type" do
        context "GET" do
          ['create', 'read', 'update', 'delete', 'grant'].each do |action|
            context "for #{action.upcase} action" do
              ['create', 'read', 'update', 'delete', 'grant'].each do |ace|

                context "an actor directly in the #{ace.upcase} ACE" do
                  with_actor :hasselhoff
                  with_entity type, :gozer

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
                  with_entity type, :gozer

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
                  with_entity type, :gozer

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
                    fake_entity = easterbunny

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
        context "PUT" do
          ['create', 'read', 'update', 'delete', 'grant'].each do |action|
            context "for #{action.upcase} action" do

              # TODO: probably want to expand this with various types of bad input,
              # although at the moment pretty much anything at all will crash it
              context "an actor directly in the GRANT ACE, with bad input" do
                with_actor :hasselhoff
                with_entity type, :gozer

                with_ace_on :gozer, :grant, :actors => [:hasselhoff]

                it "returns 400" do
                  pending "returns 500 instead" do
                    put("/#{type}s/#{gozer}/acl/#{action}",
                        :hasselhoff, :payload => {}).
                      should have_status_code(400).with_body({"error" => "bad input"})

                    get("/#{type}s/#{gozer}/acl/#{action}",
                        :superuser).should have_status_code(200).
                      with_body({"actors" => [hasselhoff], "groups" => []})
                  end
                end
              end

              # TODO: I'm not sure these are a problem or not; we may
              # want to properly error these out down the road.  Also
              # not sure we should return 400 for non-existent
              # actors/#{type}s, dunno what the right HTTP response code
              # is for that.

              context "an actor directly in the GRANT ACE, with invalid actor" do
                with_actor :hasselhoff
                with_entity type, :gozer

                with_ace_on :gozer, :grant, :actors => [:hasselhoff]

                it "returns 400" do
                  pending "returns 200 instead" do
                    put("/#{type}s/#{gozer}/acl/#{action}",
                        :hasselhoff,
                        :payload => {"actors" => ["zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz"],
                          "groups" => []}).
                      should have_status_code(400).with_body({"error" => "bad input"})

                    get("/#{type}s/#{gozer}/acl/#{action}",
                        :superuser).should have_status_code(200).
                      with_body({"actors" => [hasselhoff], "groups" => []})
                  end
                end
              end

              context "an actor directly in the GRANT ACE, with invalid group" do
                with_actor :hasselhoff
                with_entity type, :gozer

                with_ace_on :gozer, :grant, :actors => [:hasselhoff]

                it "returns 400" do
                  pending "returns 200 instead" do
                    put("/#{type}s/#{gozer}/acl/#{action}",
                        :hasselhoff, :payload => {"actors" => [],
                          "groups" => ["zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz"]}).
                      should have_status_code(400).with_body({"error" => "bad input"})

                    get("/#{type}s/#{gozer}/acl/#{action}",
                        :superuser).should have_status_code(200).
                      with_body({"actors" => [hasselhoff], "groups" => []})
                  end
                end
              end

              context "an actor directly in the GRANT ACE, with non-existent actor" do
                with_actor :hasselhoff
                with_entity type, :gozer

                with_ace_on :gozer, :grant, :actors => [:hasselhoff]

                it "returns 400" do
                  pending "returns 200 instead" do
                    put("/#{type}s/#{gozer}/acl/#{action}",
                        :hasselhoff,
                        :payload => {"actors" => ["ffffffffffffffffffffffffffffffff"],
                          "groups" => []}).
                      should have_status_code(400).with_body({"error" => "bad input"})

                    get("/#{type}s/#{gozer}/acl/#{action}",
                        :superuser).should have_status_code(200).
                      with_body({"actors" => [hasselhoff], "groups" => []})
                  end
                end
              end

              context "an actor directly in the GRANT ACE, with non-existent group" do
                with_actor :hasselhoff
                with_entity type, :gozer

                with_ace_on :gozer, :grant, :actors => [:hasselhoff]

                it "returns 400" do
                  pending "returns 200 instead" do
                    put("/#{type}s/#{gozer}/acl/#{action}",
                        :hasselhoff, :payload => {"actors" => [],
                          "groups" => ["ffffffffffffffffffffffffffffffff"]}).
                      should have_status_code(400).with_body({"error" => "bad input"})

                    get("/#{type}s/#{gozer}/acl/#{action}",
                        :superuser).should have_status_code(200).
                      with_body({"actors" => [hasselhoff], "groups" => []})
                  end
                end
              end

              context "an actor directly in the GRANT ACE, modifying actors" do
                with_actors :hasselhoff, :schwartzenegger
                with_entity type, :gozer

                with_ace_on :gozer, :grant, :actors => [:hasselhoff]

                it "can modify the ACE for actors" do
                  put("/#{type}s/#{gozer}/acl/#{action}",
                      :hasselhoff, :payload => {"actors" => [schwartzenegger],
                        "groups" => []}).
                    should have_status_code(200).with_body({})

                  get("/#{type}s/#{gozer}/acl/#{action}",
                      :superuser).should have_status_code(200).
                    with_body({"actors" => [schwartzenegger], "groups" => []})
                end
              end

              context "an actor directly in the GRANT ACE, modifying groups" do
                with_actor :hasselhoff
                with_group :brogrammers
                with_entity type, :gozer

                with_ace_on :gozer, :grant, :actors => [:hasselhoff]

                it "can modify the ACE for groups" do
                  put("/#{type}s/#{gozer}/acl/#{action}",
                      :hasselhoff, :payload => {"actors" => [],
                        "groups" => [brogrammers]}).
                    should have_status_code(200).with_body({})

                  get("/#{type}s/#{gozer}/acl/#{action}",
                      :superuser).should have_status_code(200).
                    with_body({"actors" => [], "groups" => [brogrammers]})
                end
              end

              context "an actor NOT in the GRANT ACE" do
                with_actors :malkovich, :schwartzenegger
                with_entity type, :gozer

                # Give malkovich everything EXCEPT grant
                with_acl_on :gozer, {
                  :create => {:actors => [:malkovich], :groups => []},
                  :read   => {:actors => [:malkovich], :groups => []},
                  :update => {:actors => [:malkovich], :groups => []},
                  :delete => {:actors => [:malkovich], :groups => []},
                  :grant  => {:actors => [],           :groups => []} # <--- This is it!
                }

                if (action == 'grant')
                  let(:body) { {"actors" => [], "groups" => []} }
                else
                  let(:body) { {"actors" => [malkovich], "groups" => []} }
                end

                it "cannot modify the ACE" do
                  put("/#{type}s/#{gozer}/acl/#{action}",
                      :malkovich, :payload => {"actors" => [schwartzenegger],
                        "groups" => []}).should have_status_code(403).
                    with_body({"error" => "must be in the grant access control entry to perform this action"})

                  get("/#{type}s/#{gozer}/acl/#{action}",
                      :superuser).should have_status_code(200).with_body(body)
                end
              end

              context "an actor indirectly in the GRANT ACE, modifying actors" do
                with_actors :hasselhoff, :norris
                with_group :hipsters, :actors => [:hasselhoff]
                with_entity type, :gozer

                with_ace_on :gozer, :grant, :groups => [:hipsters]

                it "can modify the ACE for actors" do
                  put("/#{type}s/#{gozer}/acl/#{action}",
                      :hasselhoff, :payload => {"actors" => [norris], "groups" => []}).
                    should have_status_code(200)
                  get("/#{type}s/#{gozer}/acl/#{action}",
                      :superuser).should have_status_code(200).
                    with_body({"actors" => [norris], "groups" => []})
                end
              end

              context "an actor indirectly in the GRANT ACE, modifying groups" do
                with_actors :hasselhoff
                with_group :hipsters, :actors => [:hasselhoff]
                with_group :brogrammers
                with_entity type, :gozer

                with_ace_on :gozer, :grant, :groups => [:hipsters]

                it "can modify the ACE for groups" do
                  put("/#{type}s/#{gozer}/acl/#{action}",
                      :hasselhoff, :payload => {"actors" => [],
                        "groups" => [brogrammers]}).should have_status_code(200)

                  get("/#{type}s/#{gozer}/acl/#{action}",
                      :superuser).should have_status_code(200).
                    with_body({"actors" => [], "groups" => [brogrammers]})
                end
              end

              context "with a non-existent target" do
                with_actor :hasselhoff

                it "can't modify its ACE, because it doesn't exist" do
                  fake_entity = easterbunny

                  # Prove it doesn't exist
                  get("/#{type}s/#{fake_entity}/acl/#{action}",
                      :hasselhoff).should have_status_code(404)

                  # Now try to modify it
                  put("/#{type}s/#{fake_entity}/acl/#{action}",
                      :hasselhoff).should have_status_code(404)
                end
              end
            end
          end
        end # PUT

        # Only actors in the DELETE ACE (directly or indirectly) can delete an object
        #
        # Deleting a non-existent object returns a 404
        #
        context "DELETE" do
          ['create', 'read', 'update', 'delete', 'grant'].each do |action|
            context "for #{action.upcase} action" do

              context "an actor directly in the GRANT ACE" do
                with_actor :hasselhoff
                with_entity type, :gozer

                with_ace_on :gozer, :grant, :actors => [:hasselhoff]

                it "can clear the ACE" do
                  pending "causes internal 500 errors" do
                    delete("/#{type}s/#{gozer}/acl/#{action}",
                           :hasselhoff).should have_status_code(200).with_body({})
                    get("/#{type}s/#{gozer}/acl/#{action}",
                        :superuser).should have_status_code(404)
                  end
                end
              end

              context "an actor NOT in the GRANT ACE" do
                with_actor :malkovich
                with_entity type, :gozer

                # Give malkovich everything EXCEPT grant
                with_acl_on :gozer, {
                  :create => {:actors => [:malkovich], :groups => []},
                  :read   => {:actors => [:malkovich], :groups => []},
                  :update => {:actors => [:malkovich], :groups => []},
                  :delete => {:actors => [:malkovich], :groups => []},
                  :grant  => {:actors => [],           :groups => []} # <--- Bingo
                }

                it "cannot clear the ACE" do
                  delete("/#{type}s/#{gozer}/acl/#{action}",
                         :malkovich).should have_status_code(403).
                    with_body({"error" => "must be in the grant access control entry to perform this action"})

                  get("/#{type}s/#{gozer}/acl/#{action}",
                      :superuser).should have_status_code(200)
                end
              end

              context "an actor indirectly in the GRANT ACE" do
                with_actor :hasselhoff
                with_group :hipsters, :actors => [:hasselhoff]
                with_entity type, :gozer

                with_ace_on :gozer, :grant, :groups => [:hipsters]

                it "can clear the ACE" do
                  pending "causes internal 500 errors" do
                    delete("/#{type}s/#{gozer}/acl/#{action}",
                           :hasselhoff).should have_status_code(200).with_body({})

                    get("/#{type}s/#{gozer}/acl/#{action}",
                        :superuser).should have_status_code(404)
                  end
                end
              end

              context "with a non-existent target" do
                with_actor :hasselhoff

                it "can't clear its ACE, because it doesn't exist" do
                  fake_entity = easterbunny

                  # Prove it doesn't exist
                  get("/#{type}s/#{fake_entity}/acl/#{action}",
                      :hasselhoff).should have_status_code(404)

                  # Now try to delete it
                  delete("/#{type}s/#{fake_entity}/acl/#{action}",
                         :hasselhoff).should have_status_code(404)
                end
              end
            end
          end
        end # DELETE
      end
    end
  end # /<type>/<id>/acl/<action>

  context "/<type>/<id>/acl/<action>/<member_type>" do
    # What we are testing:

    # These are basically null tests to verify that the server does
    # not act on incomplete requests; there are subpaths for these
    # tests that do (sometimes, more or less) work, but we're testing
    # this for completeness.

    # Might want to cut some of these out -- containers and object
    # versions should always be 404 even when an ID is specified,
    # since they can't have permissions

    [:actor, :group, :container, :object].each do |type|
      context "for #{type.upcase} type" do
        ['create', 'read', 'update', 'delete', 'grant'].each do |action|
          context "for #{action.upcase} action" do
            ['actors', 'groups', 'objects', 'containers'].each do |member_type|
              context "for #{member_type.upcase} member type" do
                with_entity type, :zuul

                it "get should not be found" do
                  get("/#{type}s/#{zuul}/acl/#{action}/#{member_type}/",
                      :superuser).should have_status_code(404)
                end

                it "post should not be found" do
                  post("/#{type}s/#{zuul}/acl/#{action}/#{member_type}/",
                       :superuser).should have_status_code(404)
                end

                it "put should not be found" do
                  put("/#{type}s/#{zuul}/acl/#{action}/#{member_type}/",
                      :superuser).should have_status_code(404)
                end

                it "delete should not be found" do
                  delete("/#{type}s/#{zuul}/acl/#{action}/#{member_type}/",
                         :superuser).should have_status_code(404)
                end
              end
            end
          end
        end
      end
    end
  end # /<type>/<id>/acl/<action>/<member_type>

  context "/<type>/<id>/acl/<action>/<member_type>/<member_id>" do
    # What we are testing:

    # Here we test via GET access to specific ACE from member_id to
    # group_id.  Apparently this returns 200 (with no body) if access
    # is available or 404 if not.  Supposedly groups are supported as
    # a member_type, but tests seem to show that only actors work
    # correctly.  We also test that a bogus member_type does not
    # return as having access.  All other HTTP verbs should be
    # disallowed.

    # Old notes:

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
    [:actor, :group, :container, :object].each do |type|
      context "for #{type.upcase} type" do
        context "GET" do
          ['create', 'read', 'update', 'delete', 'grant'].each do |action|
            context "for #{action.upcase} action" do
              ['create', 'read', 'update', 'delete', 'grant'].each do |ace|
                context "for ACTORS member type" do
                  context "an actor directly in the #{ace.upcase} ACE" do
                    with_actor :hasselhoff
                    with_entity type, :gozer

                    with_ace_on :gozer, ace.to_sym, :actors => [:hasselhoff]

                    if (action == ace)
                      it "returns 200 when in ACE" do
                        # Hasselhoff has specific ACE access on gozer
                        get("/#{type}s/#{gozer}/acl/#{action}/actors/#{hasselhoff}",
                            :hasselhoff).should have_status_code(200).with_body({})
                      end
                    else
                      it "returns 404 when not in ACE" do
                        # Hasselhoff does not have other access on gozer
                        get("/#{type}s/#{gozer}/acl/#{action}/actors/#{hasselhoff}",
                            :hasselhoff).should have_status_code(404)
                      end
                    end
                  end

                  context "an actor indirectly in the #{ace} ACE" do
                    with_actor :hasselhoff
                    with_group :hipsters, :actors => [:hasselhoff]
                    with_entity type, :gozer

                    with_ace_on :gozer, ace.to_sym, :groups => [:hipsters]

                    if (action == ace)
                      it "returns 200 when in ACE" do
                        get("/#{type}s/#{gozer}/acl/#{action}/actors/#{hasselhoff}",
                            :hasselhoff).should have_status_code(200).with_body({})
                      end
                    else
                      it "returns 404 when not in ACE" do
                        get("/#{type}s/#{gozer}/acl/#{action}/actors/#{hasselhoff}",
                            :hasselhoff).should have_status_code(404)
                      end
                    end
                  end

                  context "with a non-existent target" do
                    with_actor :hasselhoff

                    it "can't be read, because it doesn't exist" do
                      fake_entity = easterbunny

                      get("/#{type}s/#{fake_entity}/acl/#{action}/actors/#{hasselhoff}",
                          :hasselhoff).should have_status_code(404)
                    end
                  end
                end

                context "for GROUPS member type" do
                  context "a group directly in the #{ace} ACE" do
                    with_actor :hasselhoff
                    with_group :brogrammers, :actors => [:hasselhoff]
                    with_entity type, :gozer

                    with_ace_on :gozer, ace.to_sym, :groups => [:brogrammers]

                    if (action == ace)
                      it "returns 200 when in ACE" do
                        # My understanding is that if group X has permissions on actor X,
                        # this should return 200, but as it is, it doesn't
                        pending "doesn't seem to work" do
                          # Brogrammers has specific ACE access on gozer
                          get("/#{type}s/#{gozer}/acl/#{action}/groups/#{brogrammers}",
                              :hasselhoff).should have_status_code(200).with_body({})
                        end
                      end
                    else
                      it "returns 404 when not in ACE" do
                        # Brogrammers does not have other access on gozer
                        get("/#{type}s/#{gozer}/acl/#{action}/groups/#{brogrammers}",
                            :hasselhoff).should have_status_code(404)
                      end
                    end
                  end

                  context "a group indirectly in the #{ace} ACE" do
                    with_actors :hasselhoff
                    with_group :hipsters, :actors => [:hasselhoff]
                    with_group :brogrammers, :groups => [:hipsters]
                    with_entity type, :gozer

                    with_ace_on :gozer, ace.to_sym, :groups => [:brogrammers]

                    if (action == ace)
                      # See above
                      it "returns 200 when in ACE" do
                        pending "doesn't seem to work" do
                          get("/#{type}s/#{gozer}/acl/#{action}/groups/#{hipsters}",
                              :hasselhoff).should have_status_code(200).with_body({})
                        end
                      end
                    else
                      it "returns 404 when not in ACE" do
                        get("/#{type}s/#{gozer}/acl/#{action}/groups/#{hipsters}",
                            :hasselhoff).should have_status_code(404)
                      end
                    end
                  end

                  context "with a non-existent target" do
                    with_actor :hasselhoff
                    with_group :brogrammers

                    it "can't be read, because it doesn't exist" do
                      fake_entity = easterbunny

                      get("/#{type}s/#{fake_entity}/acl/#{action}/groups/#{brogrammers}",
                          :hasselhoff).should have_status_code(404)
                    end
                  end
                end

                # Some tests for an unexpected member_type -- don't really need to test
                # more than one, do we?

                context "for OBJECT member type" do
                  context "an actor directly in the #{ace} ACE" do
                    with_entity type, :gozer
                    with_object :spork

                    it "returns 404 all the time" do
                      get("/#{type}s/#{gozer}/acl/#{action}/objects/#{spork}",
                          :superuser).should have_status_code(404)
                    end
                  end

                  context "with a non-existent target" do
                    with_object :spork

                    it "can't be read, because it doesn't exist" do
                      fake_entity = easterbunny

                      get("/#{type}s/#{fake_entity}/acl/#{action}/objects/#{spork}",
                          :superuser).should have_status_code(404)
                    end
                  end
                end

                context "for CONTAINER member type" do
                  context "an actor directly in the #{ace} ACE" do
                    with_entity type, :gozer
                    with_container :chumbucket

                    it "returns 404 all the time" do
                      get("/#{type}s/#{gozer}/acl/#{action}/objects/#{chumbucket}",
                          :superuser).should have_status_code(404)
                    end
                  end

                  context "with a non-existent target" do
                    with_container :chumbucket

                    it "can't be read, because it doesn't exist" do
                      fake_entity = easterbunny

                      get("/#{type}s/#{fake_entity}/acl/#{action}/objects/#{chumbucket}",
                          :superuser).should have_status_code(404)
                    end
                  end
                end
              end
            end
          end
        end # GET

        ['create', 'read', 'update', 'delete', 'grant'].each do |action|
          context "for #{action.upcase} action" do
            ['actors', 'groups'].each do |member_type|
              context "for #{member_type.upcase} member type" do
                should_not_allow :POST, "/#{type}s/ffffffffffffffffffffffffffffffff/acl/#{action}/#{member_type}/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"
                should_not_allow :PUT, "/#{type}s/ffffffffffffffffffffffffffffffff/acl/#{action}/#{member_type}/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"
                should_not_allow :DELETE, "/#{type}s/ffffffffffffffffffffffffffffffff/acl/#{action}/#{member_type}/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"
              end
            end
          end
        end
      end
    end
  end # /<type>/<id>/acl/<action>/<member_type>/<member_id>
end
