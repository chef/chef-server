describe "Groups Endpoint" do
  let(:mattdamon) { "deadbeefdeadbeefdeadbeefdeadbeef" }
  let(:car_salesmen) { "deadbeefdeadbeefdeadbeefdeadbeef" }

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

  context "/groups/<group_id>" do
    # What we are testing:

    # Here we test group existence with GET (should require
    # appropriate READ access) and that it has the correct response,
    # as well as the ability to delete groups (should require
    # appropriate DELETE access).  All other HTTP verbs should be
    # disallowed.

    # Old notes:

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
          fake_group = car_salesmen

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
      context "an actor directly in the DELETE ACE" do
        with_actor :hasselhoff
        with_group :hipsters

        with_ace_on_group :hipsters, :delete, :actors => [:hasselhoff]

        it "can delete the group" do
          delete("/groups/#{hipsters}",
                 :hasselhoff).should have_status_code(200).with_body({})
          get("/groups/#{hipsters}", :superuser).should have_status_code(404)
        end
      end

      context "an actor NOT in the DELETE ACE" do
        with_actor :malkovich
        with_group :hipsters

        # Give malkovich everything EXCEPT delete
        with_acl_on_group :hipsters, {
          :create => {:actors => [:malkovich], :groups => []},
          :read   => {:actors => [:malkovich], :groups => []},
          :update => {:actors => [:malkovich], :groups => []},
          :delete => {:actors => [],           :groups => []}, # <--- That's the one!
          :grant  => {:actors => [:malkovich], :groups => []}
        }

        it "cannot delete the group" do
          delete("/groups/#{hipsters}", :malkovich).should have_status_code(403).
            with_body({"error" => "must be in the delete access control entry to perform this action"})
          get("/groups/#{hipsters}", :superuser).should have_status_code(200)
        end
      end

      context "an actor indirectly in the DELETE ACE" do
        with_actor :hasselhoff
        with_groups :hipsters, :brogrammers

        with_ace_on_group :brogrammers, :delete, :groups => [:hipsters]
        with_members :hipsters, :actors => [:hasselhoff]

        it "can delete the groups" do
          delete("/groups/#{brogrammers}",
                 :hasselhoff).should have_status_code(200).with_body({})
          get("/groups/#{brogrammers}", :superuser).should have_status_code(404)
        end
      end

      context "with a non-existent target" do
        with_actor :hasselhoff

        it "can't be deleted, because it doesn't exist" do
          fake_group = car_salesmen

          # Prove it doesn't exist
          get("/groups/#{fake_group}", :hasselhoff).should have_status_code(404)

          # Now try to delete it
          delete("/groups/#{fake_group}", :hasselhoff).should have_status_code(404)
        end
      end
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

  context "/groups/<group_id>/acl" do
    # What we are testing:

    # Here we test access to group's ACL and that the response body
    # has the correct format.  Apparently, any ACE at all grants
    # access to the ACL (is this a bug?) -- we test each ACE in turn,
    # both directly and indirectly through a group.  All other HTTP
    # verbs should be disallowed.

    # Old notes:

    # GET full ACL if the requesting actor (from the header) has grant
    # permission on the actor
    #
    # Cucumber: a newly-created actor should itself be present in each
    # of its own ACEs (the tests look at ACEs, not the whole ACL, but
    # they should be consistent)
    #
    # Cucumber: Additionally, a newly-created actor should contain
    # *the actor that created it* in each of its ACEs (same caveats
    # re: ACL vs. ACEs apply)

    # Old Notes:

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
      ['CREATE', 'READ', 'UPDATE', 'DELETE', 'GRANT'].each do |ace|

        context "an actor directly in the #{ace} ACE" do
          with_actor :hasselhoff
          with_group :hipsters

          with_ace_on_group :hipsters, ace.downcase.to_sym, :actors => [:hasselhoff]

          it "can read the acl" do
            body = {
              "create" => {"actors" => [], "groups" => []},
              "read" => {"actors" => [], "groups" => []},
              "update" => {"actors" => [], "groups" => []},
              "delete" => {"actors" => [], "groups" => []},
              "grant" => {"actors" => [], "groups" => []}
            }
            body[ace.downcase] = {"actors" => [hasselhoff], "groups" => []}

            get("/groups/#{hipsters}/acl",
                :hasselhoff).should have_status_code(200).with_body(body)
          end
        end

        context "an actor indirectly in the #{ace} ACE" do
          with_actor :hasselhoff
          with_groups :hipsters, :brogrammers

          with_ace_on_group :brogrammers, ace.downcase.to_sym, :groups => [:hipsters]
          with_members :hipsters, :actors => [:hasselhoff]

          it "can read the acl" do
            body = {
              "create" => {"actors" => [], "groups" => []},
              "read" => {"actors" => [], "groups" => []},
              "update" => {"actors" => [], "groups" => []},
              "delete" => {"actors" => [], "groups" => []},
              "grant" => {"actors" => [], "groups" => []}
            }
            body[ace.downcase] = {"actors" => [], "groups" => [hipsters]}

            get("/groups/#{brogrammers}/acl",
                :hasselhoff).should have_status_code(200).with_body(body)
          end
        end
      end

      context "an actor with NO ACE" do
        with_actor :malkovich
        with_group :hipsters

        # Give malkovich no access at all
        with_acl_on_group :hipsters, {
          :create => {:actors => [], :groups => []},
          :read   => {:actors => [], :groups => []},
          :update => {:actors => [], :groups => []},
          :delete => {:actors => [], :groups => []},
          :grant  => {:actors => [], :groups => []}
        }

        it "cannot read the acl" do
          get("/groups/#{hipsters}/acl", :malkovich).should have_status_code(403).
            with_body({"error" => "must be in one of the create, read, update, delete, grant access control entries to perform this action"})
        end
      end

      context "with a non-existent target" do
        with_actor :hasselhoff

        it "can't be read, because it doesn't exist" do
          fake_group = car_salesmen

          get("/groups/#{fake_group}/acl", :hasselhoff).should have_status_code(404)
        end
      end
    end # GET

    # NOTE: We'll want to eventually allow these operations in order
    # to facilitate bulk operations
    should_not_allow :POST, "/groups/ffffffffffffffffffffffffffffffff/acl"
    should_not_allow :PUT, "/groups/ffffffffffffffffffffffffffffffff/acl"
    should_not_allow :DELETE, "/groups/ffffffffffffffffffffffffffffffff/acl"
  end # /groups/<group_id>/acl

  # Manipulate a specific permission on a given group
  context "/groups/<group_id>/acl/<action>" do
    # What we are testing:

    # Here we test access to a specific ACE/action in group's ACL and
    # that the response body has the correct format.  Apparently, any
    # ACE at all grants access to the ACL (is this a bug?) -- we test
    # each ACE in turn, both directly and indirectly through a group.
    # PUT is used for updating the ACL and is likewise tested
    # (although there is currently no checking for request
    # correctness, and authz will crash on badly formatted requests).
    # DELETE is also tested, however it seems to be broken.  HTTP POST
    # should be disallowed.

    # Old notes:

    # GET actors and groups for action
    #
    # Cucumber: the group's creator (i.e., the one with GRANT
    # privileges) can read every ACE
    context "GET" do
      ['CREATE', 'READ', 'UPDATE', 'DELETE', 'GRANT'].each do |action|
        context "for #{action} action" do

          ['CREATE', 'READ', 'UPDATE', 'DELETE', 'GRANT'].each do |ace|

            context "an actor directly in the #{ace} ACE" do
              with_actor :hasselhoff
              with_group :hipsters

              with_ace_on_group :hipsters, ace.downcase.to_sym, :actors => [:hasselhoff]

              if (action == ace)
                let(:body) { {"actors" => [hasselhoff], "groups" => []} }
              else
                let(:body) { {"actors" => [], "groups" => []} }
              end

              it "can read the acl" do
                get("/groups/#{hipsters}/acl/#{action}",
                    :hasselhoff).should have_status_code(200).with_body(body)
              end
            end

            context "an actor indirectly in the #{ace} ACE" do
              with_actor :hasselhoff
              with_groups :hipsters, :brogrammers

              with_ace_on_group :brogrammers, ace.downcase.to_sym, :groups => [:hipsters]
              with_members :hipsters, :actors => [:hasselhoff]

              if (action == ace)
                let(:body) { {"actors" => [], "groups" => [hipsters]} }
              else
                let(:body) { {"actors" => [], "groups" => []} }
              end

              it "can read the acl" do
                get("/groups/#{brogrammers}/acl/#{action}",
                    :hasselhoff).should have_status_code(200).with_body(body)
              end
            end

            context "an actor with NO ACE" do
              with_actor :malkovich
              with_group :hipsters

              # Give malkovich no access at all
              with_acl_on_group :hipsters, {
                :create => {:actors => [], :groups => []},
                :read   => {:actors => [], :groups => []},
                :update => {:actors => [], :groups => []},
                :delete => {:actors => [], :groups => []},
                :grant  => {:actors => [], :groups => []}
              }

              it "cannot read the acl" do
                get("/groups/#{hipsters}/acl/#{action}",
                    :malkovich).should have_status_code(403).with_body({"error" => "must be in one of the create, read, update, delete, grant access control entries to perform this action"})
              end
            end

            context "with a non-existent target" do
              with_actor :hasselhoff

              it "can't be read, because it doesn't exist" do
                fake_group = car_salesmen

                get("/groups/#{fake_group}/acl/#{action}",
                    :hasselhoff).should have_status_code(404)
              end
            end
          end
        end
      end
    end # GET

    ['create', 'read', 'update', 'delete', 'grant'].each do |action|
      context "for #{action.upcase} action" do
        should_not_allow :POST, "/groups/ffffffffffffffffffffffffffffffff/acl/#{action}"
      end
    end

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
      ['CREATE', 'READ', 'UPDATE', 'DELETE', 'GRANT'].each do |action|
        context "for #{action} action" do

          # TODO: probably want to expand this with various types of bad input,
          # although at the moment pretty much anything at all will crash it
          context "an actor directly in the GRANT ACE, with bad input" do
            with_actor :hasselhoff
            with_group :hipsters

            with_ace_on_group :hipsters, :grant, :actors => [:hasselhoff]

            it "returns 400" do
              pending "returns 500 instead" do
                put("/groups/#{hipsters}/acl/#{action.downcase}",
                    :hasselhoff, :payload => {}).
                  should have_status_code(400).with_body({"error" => "bad input"})

                get("/groups/#{hipsters}/acl/#{action.downcase}",
                    :superuser).should have_status_code(200).
                  with_body({"actors" => [hasselhoff], "groups" => []})
              end
            end
          end

          # TODO: I'm not sure these are a problem or not; we may want to properly
          # error these out down the road.  Also not sure we should return 400 for
          # non-existent actors/groups, dunno what the right HTTP response code is
          # for that.

          context "an actor directly in the GRANT ACE, with invalid actor" do
            with_actor :hasselhoff
            with_group :hipsters

            with_ace_on_group :hipsters, :grant, :actors => [:hasselhoff]

            it "returns 400" do
              pending "returns 200 instead" do
                put("/groups/#{hipsters}/acl/#{action.downcase}",
                    :hasselhoff,
                    :payload => {"actors" => ["zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz"],
                      "groups" => []}).
                  should have_status_code(400).with_body({"error" => "bad input"})

                get("/groups/#{hipsters}/acl/#{action.downcase}",
                    :superuser).should have_status_code(200).
                  with_body({"actors" => [hasselhoff], "groups" => []})
              end
            end
          end

          context "an actor directly in the GRANT ACE, with invalid group" do
            with_actor :hasselhoff
            with_group :hipsters

            with_ace_on_group :hipsters, :grant, :actors => [:hasselhoff]

            it "returns 400" do
              pending "returns 200 instead" do
                put("/groups/#{hipsters}/acl/#{action.downcase}",
                    :hasselhoff, :payload => {"actors" => [],
                      "groups" => ["zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz"]}).
                  should have_status_code(400).with_body({"error" => "bad input"})

                get("/groups/#{hipsters}/acl/#{action.downcase}",
                    :superuser).should have_status_code(200).
                  with_body({"actors" => [hasselhoff], "groups" => []})
              end
            end
          end

          context "an actor directly in the GRANT ACE, with non-existent actor" do
            with_actor :hasselhoff
            with_group :hipsters

            with_ace_on_group :hipsters, :grant, :actors => [:hasselhoff]

            it "returns 400" do
              pending "returns 200 instead" do
                put("/groups/#{hipsters}/acl/#{action.downcase}",
                    :hasselhoff,
                    :payload => {"actors" => ["ffffffffffffffffffffffffffffffff"],
                      "groups" => []}).
                  should have_status_code(400).with_body({"error" => "bad input"})

                get("/groups/#{hipsters}/acl/#{action.downcase}",
                    :superuser).should have_status_code(200).
                  with_body({"actors" => [hasselhoff], "groups" => []})
              end
            end
          end

          context "an actor directly in the GRANT ACE, with non-existent group" do
            with_actor :hasselhoff
            with_group :hipsters

            with_ace_on_group :hipsters, :grant, :actors => [:hasselhoff]

            it "returns 400" do
              pending "returns 200 instead" do
                put("/groups/#{hipsters}/acl/#{action.downcase}",
                    :hasselhoff, :payload => {"actors" => [],
                      "groups" => ["ffffffffffffffffffffffffffffffff"]}).
                  should have_status_code(400).with_body({"error" => "bad input"})

                get("/groups/#{hipsters}/acl/#{action.downcase}",
                    :superuser).should have_status_code(200).
                  with_body({"actors" => [hasselhoff], "groups" => []})
              end
            end
          end

          context "an actor directly in the GRANT ACE, modifying actors" do
            with_actors :hasselhoff, :schwartzenegger
            with_group :hipsters

            with_ace_on_group :hipsters, :grant, :actors => [:hasselhoff]

            it "can modify the ACE for actors" do
              put("/groups/#{hipsters}/acl/#{action.downcase}",
                  :hasselhoff, :payload => {"actors" => [schwartzenegger],
                    "groups" => []}).
                should have_status_code(200).with_body({})

              get("/groups/#{hipsters}/acl/#{action.downcase}",
                  :superuser).should have_status_code(200).
                with_body({"actors" => [schwartzenegger], "groups" => []})
            end
          end

          context "an actor directly in the GRANT ACE, modifying groups" do
            with_actor :hasselhoff
            with_groups :brogrammers, :hipsters

            with_ace_on_group :hipsters, :grant, :actors => [:hasselhoff]

            it "can modify the ACE for groups" do
              put("/groups/#{hipsters}/acl/#{action.downcase}",
                  :hasselhoff, :payload => {"actors" => [], "groups" => [brogrammers]}).
                should have_status_code(200).with_body({})

              get("/groups/#{hipsters}/acl/#{action.downcase}",
                  :superuser).should have_status_code(200).
                with_body({"actors" => [], "groups" => [brogrammers]})
            end
          end

          context "an actor NOT in the GRANT ACE" do
            with_actors :malkovich, :schwartzenegger
            with_group :hipsters

            # Give malkovich everything EXCEPT grant
            with_acl_on_group :hipsters, {
              :create => {:actors => [:malkovich], :groups => []},
              :read   => {:actors => [:malkovich], :groups => []},
              :update => {:actors => [:malkovich], :groups => []},
              :delete => {:actors => [:malkovich], :groups => []},
              :grant  => {:actors => [],           :groups => []} # <--- That's the one!
            }

            if (action == 'GRANT')
              let(:body) { {"actors" => [], "groups" => []} }
            else
              let(:body) { {"actors" => [malkovich], "groups" => []} }
            end

            it "cannot modify the ACE" do
              put("/groups/#{hipsters}/acl/#{action.downcase}",
                  :malkovich, :payload => {"actors" => [schwartzenegger],
                    "groups" => []}).should have_status_code(403).
                with_body({"error" => "must be in the grant access control entry to perform this action"})

              get("/groups/#{hipsters}/acl/#{action.downcase}",
                  :superuser).should have_status_code(200).with_body(body)
            end
          end

          context "an actor indirectly in the GRANT ACE, modifying actors" do
            with_actors :hasselhoff, :malkovich, :norris
            with_groups :hipsters, :brogrammers

            with_ace_on_group :brogrammers, :grant, :groups => [:hipsters]
            with_members :hipsters, :actors => [:hasselhoff]

            it "can modify the ACE for actors" do
              put("/groups/#{brogrammers}/acl/#{action.downcase}",
                     :hasselhoff, :payload => {"actors" => [norris], "groups" => []}).
                should have_status_code(200)
              get("/groups/#{brogrammers}/acl/#{action.downcase}",
                  :superuser).should have_status_code(200).
                with_body({"actors" => [norris], "groups" => []})
            end
          end

          context "an actor indirectly in the GRANT ACE, modifying groups" do
            with_actors :hasselhoff, :malkovich
            with_groups :hipsters, :brogrammers, :commies

            with_ace_on_group :commies, :grant, :groups => [:hipsters]
            with_members :hipsters, :actors => [:hasselhoff]

            it "can modify the ACE for groups" do
              put("/groups/#{commies}/acl/#{action.downcase}",
                     :hasselhoff, :payload => {"actors" => [],
                    "groups" => [brogrammers]}).should have_status_code(200)

              get("/groups/#{commies}/acl/#{action.downcase}",
                  :superuser).should have_status_code(200).
                with_body({"actors" => [], "groups" => [brogrammers]})
            end
          end

          context "with a non-existent target" do
            with_actor :hasselhoff

            it "can't modify its ACE, because it doesn't exist" do
              fake_group = car_salesmen

              # Prove it doesn't exist
              get("/groups/#{fake_group}/acl/#{action.downcase}",
                  :hasselhoff).should have_status_code(404)

              # Now try to modify it
              put("/groups/#{fake_group}/acl/#{action.downcase}",
                  :hasselhoff).should have_status_code(404)
            end
          end
        end
      end
    end # GET

    # DELETE clears actors and groups from ACE
    context "DELETE" do
      ['CREATE', 'READ', 'UPDATE', 'DELETE', 'GRANT'].each do |action|
        context "for #{action} action" do

          context "an actor directly in the GRANT ACE" do
            with_actor :hasselhoff
            with_group :hipsters

            with_ace_on_group :hipsters, :grant, :actors => [:hasselhoff]

            it "can clear the ACE" do
              pending "causes internal 500 errors" do
                delete("/groups/#{hipsters}/acl/#{action.downcase}",
                       :hasselhoff).should have_status_code(200).with_body({})
                get("/groups/#{hipsters}/acl/#{action.downcase}",
                    :superuser).should have_status_code(404)
              end
            end
          end

          context "an actor NOT in the GRANT ACE" do
            with_actor :malkovich
            with_group :hipsters

            # Give malkovich everything EXCEPT grant
            with_acl_on_group :hipsters, {
              :create => {:actors => [:malkovich], :groups => []},
              :read   => {:actors => [:malkovich], :groups => []},
              :update => {:actors => [:malkovich], :groups => []},
              :delete => {:actors => [:malkovich], :groups => []},
              :grant  => {:actors => [],           :groups => []} # <--- That's the one!
            }

            it "cannot clear the ACE" do
              delete("/groups/#{hipsters}/acl/#{action.downcase}",
                     :malkovich).should have_status_code(403).
                with_body({"error" => "must be in the grant access control entry to perform this action"})

              get("/groups/#{hipsters}/acl/#{action.downcase}",
                  :superuser).should have_status_code(200)
            end
          end

          context "an actor indirectly in the GRANT ACE" do
            with_actor :hasselhoff
            with_groups :hipsters, :brogrammers

            with_ace_on_group :brogrammers, :grant, :groups => [:hipsters]
            with_members :hipsters, :actors => [:hasselhoff]

            it "can clear the ACE" do
              pending "causes internal 500 errors" do
                delete("/groups/#{brogrammers}/acl/#{action.downcase}",
                       :hasselhoff).should have_status_code(200).with_body({})

                get("/groups/#{brogrammers}/acl/#{action.downcase}",
                    :superuser).should have_status_code(404)
              end
            end
          end

          context "with a non-existent target" do
            with_actor :hasselhoff

            it "can't clear its ACE, because it doesn't exist" do
              fake_group = car_salesmen

              # Prove it doesn't exist
              get("/groups/#{fake_group}/acl/#{action.downcase}",
                  :hasselhoff).should have_status_code(404)

              # Now try to delete it
              delete("/groups/#{fake_group}/acl/#{action.downcase}",
                     :hasselhoff).should have_status_code(404)
            end
          end
        end
      end
    end # GET
  end # /groups/<group_id>/acl/<action>

  context "/groups/<group_id>/acl/<action>/<member_type>" do
    # What we are testing:

    # These are basically null tests to verify that the server does
    # not act on incomplete requests; there are subpaths for these
    # tests that do (sometimes, more or less) work, but we're testing
    # this for completeness.

    # Might want to cut some of these out -- containers and object
    # versions should always be 404 even when an ID is specified,
    # since they can't have permissions

    ['CREATE', 'READ', 'UPDATE', 'DELETE', 'GRANT'].each do |action|
      context "for #{action} action" do
        ['ACTORS', 'GROUPS', 'OBJECTS', 'CONTAINERS'].each do |type|
          context "for #{type} member type" do
            with_group :commies

            it "get should not be found" do
              get("/groups/#{commies}/acl/#{action.downcase}/#{type.downcase}/",
                  :superuser).should have_status_code(404)
            end

            it "post should not be found" do
              post("/groups/#{commies}/acl/#{action.downcase}/#{type.downcase}/",
                   :superuser).should have_status_code(404)
            end

            it "put should not be found" do
              put("/groups/#{commies}/acl/#{action.downcase}/#{type.downcase}/",
                  :superuser).should have_status_code(404)
            end

            it "delete should not be found" do
              delete("/groups/#{commies}/acl/#{action.downcase}/#{type.downcase}/",
                     :superuser).should have_status_code(404)
            end
          end
        end
      end
    end
  end # /actors/<actor_id>/acl/<action>/<member_type>

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
