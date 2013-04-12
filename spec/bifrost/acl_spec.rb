describe "ACL Tests" do
  let(:easterbunny) { "deaddeaddeaddeaddeaddeaddeaddead" }
  let(:max_headroom) { "deadbeefdeadbeefdeadbeefdeadbeef" }

  context "/<type>/<id>/acl" do
    # What we are testing:

    # Here we test access to type's ACL and that the response body
    # has the correct format.  Apparently, any ACE at all grants
    # access to the ACL (is this a good idea?[1]) -- we test each ACE in
    # turn, both directly and indirectly through a group.  All other
    # HTTP verbs should be disallowed.

    # [1] Apparently done intentionally, but the reasons are lost to the
    # mists of time.  Would be nice to find those reasons again.
    context "GET" do
      [:group, :container, :object].each do |type|
        context "for #{type.upcase} type" do
          ['create', 'read', 'update', 'delete', 'grant'].each do |ace|

            context "an actor directly in the #{ace.upcase} ACE" do
              with_actor :hasselhoff
              with_entity type, :gozer

              with_ace_on :gozer, ace.to_sym, :to => :hasselhoff

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
              with_group :hipsters, :members => [:hasselhoff]
              with_entity type, :gozer

              with_ace_on :gozer, ace.to_sym, :to => :hipsters

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
        end
      end
    end # GET

    [:group, :container, :object].each do |type|
      context "for #{type.upcase} type" do
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

    context "GET" do
      [:group, :container, :object].each do |type|
        context "for #{type.upcase} type" do

          ['create', 'read', 'update', 'delete', 'grant'].each do |action|
            context "for #{action.upcase} action" do

              ['create', 'read', 'update', 'delete', 'grant'].each do |ace|

                context "an actor directly in the #{ace.upcase} ACE" do
                  with_actor :hasselhoff
                  with_entity type, :gozer

                  with_ace_on :gozer, ace.to_sym, :to => :hasselhoff

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
                  with_group :hipsters, :members => [:hasselhoff]
                  with_entity type, :gozer

                  with_ace_on :gozer, ace.to_sym, :to => :hipsters

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
        end
      end
    end # GET

    [:group, :container, :object].each do |type|
      context "for #{type.upcase} type" do
        ['create', 'read', 'update', 'delete', 'grant'].each do |action|
          context "for #{action.upcase} action" do
            should_not_allow :POST, "/#{type}s/ffffffffffffffffffffffffffffffff/acl/#{action}"
          end
        end
      end
    end

    # PUT replaces an ACE atomically
    context "PUT" do
      [:group, :container, :object].each do |type|
        context "for #{type.upcase} type" do

          ['create', 'read', 'update', 'delete', 'grant'].each do |action|
            context "for #{action.upcase} action" do

              # TODO: probably want to expand this with various types of bad input,
              # although at the moment pretty much anything at all will crash it
              context "an actor directly in the GRANT ACE, with bad input" do
                with_actor :hasselhoff
                with_entity type, :gozer

                with_ace_on :gozer, :grant, :to => :hasselhoff

                it "returns 400" do
                  put("/#{type}s/#{gozer}/acl/#{action}",
                    :hasselhoff, :payload => {}).
                    should have_status_code(400).
                    with_body({"error" => "invalid JSON in request body"})

                  if (action == 'grant')
                    response_body = {"actors" => [hasselhoff], "groups" => []}
                  else
                    response_body = {"actors" => [], "groups" => []}
                  end

                  get("/#{type}s/#{gozer}/acl/#{action}",
                    :superuser).should have_status_code(200).
                    with_body(response_body)
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

                with_ace_on :gozer, :grant, :to => :hasselhoff

                it "returns 400" do
                  put("/#{type}s/#{gozer}/acl/#{action}",
                    :hasselhoff,
                    :payload => {"actors" => ["zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz"],
                      "groups" => []}).
                    should have_status_code(400).
                    with_body({"error" => "attempt to add non-existent actor 'zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz' to ACL"})

                  if (action == 'grant')
                    body = {"actors" => [hasselhoff], "groups" => []}
                  else
                    body = {"actors" => [], "groups" => []}
                  end

                  get("/#{type}s/#{gozer}/acl/#{action}",
                    :superuser).should have_status_code(200).
                    with_body(body)
                end
              end

              context "an actor directly in the GRANT ACE, with invalid group" do
                with_actor :hasselhoff
                with_entity type, :gozer

                with_ace_on :gozer, :grant, :to => :hasselhoff

                it "returns 400" do
                  put("/#{type}s/#{gozer}/acl/#{action}",
                    :hasselhoff, :payload => {"actors" => [],
                      "groups" => ["zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz"]}).
                    should have_status_code(400).
                    with_body({"error" => "attempt to add non-existent group 'zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz' to ACL"})

                  if (action == 'grant')
                    body = {"actors" => [hasselhoff], "groups" => []}
                  else
                    body = {"actors" => [], "groups" => []}
                  end

                  get("/#{type}s/#{gozer}/acl/#{action}",
                    :superuser).should have_status_code(200).
                    with_body(body)
                end
              end

              context "an actor directly in the GRANT ACE, with non-existent actor" do
                with_actor :hasselhoff
                with_entity type, :gozer

                with_ace_on :gozer, :grant, :to => :hasselhoff

                it "returns 400" do
                  put("/#{type}s/#{gozer}/acl/#{action}",
                    :hasselhoff,
                    :payload => {"actors" => ["ffffffffffffffffffffffffffffffff"],
                      "groups" => []}).
                    should have_status_code(400).
                    with_body({"error" => "attempt to add non-existent actor 'ffffffffffffffffffffffffffffffff' to ACL"})

                  if (action == 'grant')
                    body = {"actors" => [hasselhoff], "groups" => []}
                  else
                    body = {"actors" => [], "groups" => []}
                  end

                  get("/#{type}s/#{gozer}/acl/#{action}",
                    :superuser).should have_status_code(200).
                    with_body(body)
                end
              end

              context "an actor directly in the GRANT ACE, with non-existent group" do
                with_actor :hasselhoff
                with_entity type, :gozer

                with_ace_on :gozer, :grant, :to => :hasselhoff

                it "returns 400" do
                  put("/#{type}s/#{gozer}/acl/#{action}",
                    :hasselhoff, :payload => {"actors" => [],
                      "groups" => ["ffffffffffffffffffffffffffffffff"]}).
                    should have_status_code(400).
                    with_body({"error" => "attempt to add non-existent group 'ffffffffffffffffffffffffffffffff' to ACL"})

                  if (action == 'grant')
                    body = {"actors" => [hasselhoff], "groups" => []}
                  else
                    body = {"actors" => [], "groups" => []}
                  end

                  get("/#{type}s/#{gozer}/acl/#{action}",
                    :superuser).should have_status_code(200).
                    with_body(body)
                end
              end

              context "an actor directly in the GRANT ACE, modifying actors" do
                with_actors :hasselhoff, :schwartzenegger
                with_entity type, :gozer

                with_ace_on :gozer, :grant, :to => :hasselhoff

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

                with_ace_on :gozer, :grant, :to => :hasselhoff

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
                with_group :hipsters, :members => [:hasselhoff]
                with_entity type, :gozer

                with_ace_on :gozer, :grant, :to => :hipsters

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
                with_group :hipsters, :members => [:hasselhoff]
                with_group :brogrammers
                with_entity type, :gozer

                with_ace_on :gozer, :grant, :to => :hipsters

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
        end
      end
    end # PUT

    context "DELETE" do
      [:group, :container, :object].each do |type|
        context "for #{type.upcase} type" do

          ['create', 'read', 'update', 'delete', 'grant'].each do |action|
            context "for #{action.upcase} action" do

              context "an actor directly in the GRANT ACE" do
                with_actor :hasselhoff
                with_entity type, :gozer

                with_ace_on :gozer, :grant, :to => :hasselhoff

                it "can clear the ACE" do
                  delete("/#{type}s/#{gozer}/acl/#{action}",
                    :hasselhoff).should have_status_code(200).with_body({})
                  get("/#{type}s/#{gozer}/acl/#{action}",
                    :superuser).should have_status_code(200).
                    with_body({"actors" => [], "groups" => []})
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

                  if (action == "grant")
                    response_body = {"actors" => [], "groups" => []}
                  else
                    response_body = {"actors" => [malkovich], "groups" => []}
                  end

                  get("/#{type}s/#{gozer}/acl/#{action}",
                      :superuser).should have_status_code(200).
                    with_body(response_body)
                end
              end

              context "an actor indirectly in the GRANT ACE" do
                with_actor :hasselhoff
                with_group :hipsters, :members => [:hasselhoff]
                with_entity type, :gozer

                with_ace_on :gozer, :grant, :to => :hipsters

                it "can clear the ACE" do
                  delete("/#{type}s/#{gozer}/acl/#{action}",
                    :hasselhoff).should have_status_code(200).with_body({})

                  response_body = {"actors" => [], "groups" => []}

                  get("/#{type}s/#{gozer}/acl/#{action}",
                    :superuser).should have_status_code(200).
                    with_body(response_body)
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
        end
      end
    end # DELETE
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
    # (<type>)_id.  Apparently this returns 200 (with no body) if
    # access is available or 404 if not.  Supposedly groups are
    # supported as a member_type, but tests seem to show that only
    # actors work correctly.  We also test that a bogus member_type
    # does not return as having access.  All other HTTP verbs should
    # be disallowed.

    # TODO: Perhaps use 204 (OK, No Content) instead?
    #
    # Deleting one group from a given ACE shouldn't affect other
    # holders of that permission that were not in that group (however
    # if an actor is in multiple groups, and one of them is removed,
    # that actor should still have the permission by virtue of their
    # other group membership!  This wasn't tested in Cuke.)
    # TODO:  Test this?  Probably not the right context though.
    context "GET" do
      [:actor, :group, :container, :object].each do |type|
        context "for #{type.upcase} type" do

          ['create', 'read', 'update', 'delete', 'grant'].each do |action|
            context "for #{action.upcase} action" do

              ['create', 'read', 'update', 'delete', 'grant'].each do |ace|
                context "for ACTORS member type" do
                  context "an actor directly in the #{ace.upcase} ACE" do
                    with_actor :hasselhoff
                    with_entity type, :gozer

                    with_ace_on :gozer, ace.to_sym, :to => :hasselhoff

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

                  context "an actor indirectly in the #{ace.upcase} ACE" do
                    with_actor :hasselhoff
                    with_group :hipsters, :members => [:hasselhoff]
                    with_entity type, :gozer

                    with_ace_on :gozer, ace.to_sym, :to => :hipsters

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

                  context "when #{ace.upcase} ACE requested by third party" do
                    with_actors :hasselhoff, :shatner
                    with_entity type, :gozer

                    with_ace_on :gozer, ace.to_sym, :to => :hasselhoff

                    if (action == ace)
                      it "returns 200 when in ACE" do
                        # Hasselhoff has specific ACE access on gozer
                        get("/#{type}s/#{gozer}/acl/#{action}/actors/#{hasselhoff}",
                          :shatner).should have_status_code(200).with_body({})
                      end
                    else
                      it "returns 404 when not in ACE" do
                        # Hasselhoff does not have other access on gozer
                        get("/#{type}s/#{gozer}/acl/#{action}/actors/#{hasselhoff}",
                            :shatner).should have_status_code(404)
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

                  context "with a non-existent authorizee" do
                    with_actor :hasselhoff
                    with_entity type, :gozer

                    with_ace_on :gozer, ace.to_sym, :to => :hasselhoff

                    it "can't be read, because actor doesn't exist" do
                      fake_actor = max_headroom

                      get("/#{type}s/#{gozer}/acl/#{action}/actors/#{max_headroom}",
                          :hasselhoff).should have_status_code(404)
                    end
                  end
                end

                context "for GROUPS member type" do
                  context "a group directly in the #{ace} ACE" do
                    with_actor :hasselhoff
                    with_group :brogrammers, :members => [:hasselhoff]
                    with_entity type, :gozer

                    with_ace_on :gozer, ace.to_sym, :to => :brogrammers

                    it "always returns 404" do
                      # Brogrammers does not have other access on gozer
                      get("/#{type}s/#{gozer}/acl/#{action}/groups/#{brogrammers}",
                        :hasselhoff).should have_status_code(404)
                    end
                  end

                  context "a group indirectly in the #{ace} ACE" do
                    with_actors :hasselhoff
                    with_group :hipsters, :members => [:hasselhoff]
                    with_group :brogrammers, :members => [:hipsters]
                    with_entity type, :gozer

                    with_ace_on :gozer, ace.to_sym, :to => :brogrammers

                    it "always returns 404" do
                      get("/#{type}s/#{gozer}/acl/#{action}/groups/#{hipsters}",
                        :hasselhoff).should have_status_code(404)
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

                # Some tests for an unexpected member_type -- probably
                # don't really need to test more than this, do we?

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
        end
      end
    end # GET

    [:actor, :group, :container, :object].each do |type|
      context "for #{type.upcase} type" do
        ['create', 'read', 'update', 'delete', 'grant'].each do |action|
          context "for #{action.upcase} action" do
            context "for ACTORS member type" do
              should_not_allow :POST, "/#{type}s/ffffffffffffffffffffffffffffffff/acl/#{action}/actors/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"
              should_not_allow :PUT, "/#{type}s/ffffffffffffffffffffffffffffffff/acl/#{action}/actors/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"
              should_not_allow :DELETE, "/#{type}s/ffffffffffffffffffffffffffffffff/acl/#{action}/actors/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"
            end
          end
        end
      end
    end
    [:actor, :group, :container, :object].each do |type|
      context "for #{type.upcase} type" do
        ['create', 'read', 'update', 'delete', 'grant'].each do |action|
          context "for #{action.upcase} action" do
            ['groups', 'containers', 'objects'].each do |member_type|
              context "for #{member_type.upcase} member type" do
                it "returns 404" do
                  post("/#{type}s/ffffffffffffffffffffffffffffffff/acl/#{action}/#{member_type}/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee",
                    :superuser).should have_status_code(404)
                end
                it "returns 404" do
                  put("/#{type}s/ffffffffffffffffffffffffffffffff/acl/#{action}/#{member_type}/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee",
                    :superuser).should have_status_code(404)
                end
                it "returns 404" do
                  delete("/#{type}s/ffffffffffffffffffffffffffffffff/acl/#{action}/#{member_type}/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee",
                    :superuser).should have_status_code(404)
                end
              end
            end
          end
        end
      end
    end
  end # /<type>/<id>/acl/<action>/<member_type>/<member_id>
end
