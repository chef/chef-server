describe "Groups Endpoint" do
  let(:mattdamon) { "deadbeefdeadbeefdeadbeefdeadbeef" }
  let(:car_salesmen) { "deadbeefdeadbeefdeadbeefdeadbeef" }

  context "/groups" do
    # What we are testing:

    # Here we test group creation (all other HTTP verbs should be
    # disallowed), making sure the response body is correct and that
    # id and id in the uri match, as well as basic header validation,
    # as well as making sure that the requesting actor is contained in
    # the newly created group's ACLs.

    should_not_allow :GET, "/groups"

    # POST creates a new group and its ACL, creating and pre-populating
    # its ACEs with the requesting actor
    #
    # Returns a standard id/uri JSON object
    #
    # No body is accepted; currently groups are not created with any
    # members (this would be a good feature to add for speeding up
    # Authz).
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

    # Note that only DIRECT membership is reflected in group
    # membership lists (i.e., if an actor X is a member of group A,
    # which itself is a member of group B, then X will not appear in
    # the actors list of group B, even though X behaves as though it
    # does)
    context "GET" do
      context "an actor directly in the READ ACE" do
        with_actor :hasselhoff
        with_group :hipsters

        with_ace_on :hipsters, :read, :actors => [:hasselhoff]

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
        with_acl_on :hipsters, {
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
        with_group :hipsters, :actors => [:hasselhoff]
        with_group :brogrammers

        with_ace_on :brogrammers, :read, :groups => [:hipsters]

        it "can read the group" do
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

    context "DELETE" do
      context "an actor directly in the DELETE ACE" do
        with_actor :hasselhoff
        with_group :hipsters

        with_ace_on :hipsters, :delete, :actors => [:hasselhoff]

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
        with_acl_on :hipsters, {
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
        with_group :hipsters, :actors => [:hasselhoff]
        with_group :brogrammers

        with_ace_on :brogrammers, :delete, :groups => [:hipsters]

        it "can delete the group" do
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

  context "/groups/<group_id>/<member_type>" do
    # What we are testing:

    # These are basically null tests to verify that the server does
    # not act on incomplete requests; there are subpaths for these
    # tests that do (sometimes, more or less) work, but we're testing
    # this for completeness.

    # Might want to cut some of these out -- containers and object
    # versions should always be 404 even when an ID is specified,
    # since they can't be members of a group

    ['ACTORS', 'GROUPS', 'OBJECTS', 'CONTAINERS'].each do |type|
      context "for #{type} member type" do
        with_group :commies

        it "get should not be found" do
          get("/groups/#{commies}/#{type.downcase}",
              :superuser).should have_status_code(404)
        end

        it "post should not be found" do
          post("/groups/#{commies}/#{type.downcase}",
               :superuser).should have_status_code(404)
        end

        it "put should not be found" do
          put("/groups/#{commies}/#{type.downcase}",
              :superuser).should have_status_code(404)
        end

        it "delete should not be found" do
          delete("/groups/#{commies}/#{type.downcase}",
                 :superuser).should have_status_code(404)
        end
      end
    end
  end # /groups/<group_id>/<member_type>

  context "/groups/<group_id>/<member_type>/<member_id>" do
    # What we are testing:

    # Here we test that groups and actor members of groups can be
    # added with PUT and removed with DELETE (and that the other
    # endpoints are disallowed).  We explicitly test several edge
    # scenarios, such as adding groups/actors to groups they already
    # belong to and deleting them from groups they don't belong to.
    # We're also testing for cycles being forbidden (which they aren't
    # in the old API, so those tests are pending for now).

    ['actors', 'groups'].each do |type|
      context "for #{type.upcase} type" do
        should_not_allow :GET, "/groups/ffffffffffffffffffffffffffffffff/#{type}/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"
        should_not_allow :POST, "/groups/ffffffffffffffffffffffffffffffff/#{type}/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"
      end
    end

    # Note: In the below tests, using a non-existent group_id should
    # return 404, NOT 500!  See
    # https://tickets.corp.opscode.com/browse/PL-536

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
    # The old version of authz allows cycles and groups to be members
    # of themselves, however will will NOT be allowing that with the
    # new version.
    context "PUT" do
      context "for actors" do
        context "an actor directly in the UPDATE ACE" do
          with_actor :shatner
          with_group :hipsters

          with_ace_on :hipsters, :update, :actors => [:shatner]

          it "can add a user to the group" do
            put("/groups/#{hipsters}/actors/#{shatner}",
                :shatner).should have_status_code(200).with_body({})
            get("/groups/#{hipsters}", :superuser).should have_status_code(200).
              with_body({"actors" => [shatner], "groups" => []})
          end
        end

        # Because this has side effects, we have to have a new context so
        # that the group membership is properly (re-?)initialized
        context "an actor directly in the UPDATE ACE (2)" do
          with_actor :shatner
          with_group :hipsters

          with_ace_on :hipsters, :update, :actors => [:shatner]

          it "can add the same user to the group" do
            put("/groups/#{hipsters}/actors/#{shatner}",
                :shatner).should have_status_code(200).with_body({})
            get("/groups/#{hipsters}", :superuser).should have_status_code(200).
              with_body({"actors" => [shatner], "groups" => []})

            put("/groups/#{hipsters}/actors/#{shatner}",
                :shatner).should have_status_code(200).with_body({})
            get("/groups/#{hipsters}", :superuser).should have_status_code(200).
              with_body({"actors" => [shatner], "groups" => []})
          end
        end

        context "an actor directly in the UPDATE ACE (3)" do
          with_actor :shatner
          with_group :hipsters

          with_ace_on :hipsters, :update, :actors => [:shatner]

          it "adding a bogus user raises an error" do
            bogus_actor = "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz"

            put("/groups/#{hipsters}/actors/#{bogus_actor}",
                :shatner).should have_status_code(403).
              with_body({"error" => "invalid actor"})
            get("/groups/#{hipsters}", :superuser).should have_status_code(200).
              with_body({"actors" => [], "groups" => []})
          end

          it "adding a non-existent user raises an error" do
            fake_actor = mattdamon

            put("/groups/#{hipsters}/actors/#{fake_actor}",
                :shatner).should have_status_code(403).
              with_body({"error" => "invalid actor"})
            get("/groups/#{hipsters}", :superuser).should have_status_code(200).
              with_body({"actors" => [], "groups" => []})
          end
        end

        context "an actor NOT in the UPDATE ACE" do
          with_actor :shatner
          with_group :hipsters

          # Give shatner everything EXCEPT update
          with_acl_on :hipsters, {
            :create => {:actors => [:shatner], :groups => []},
            :read   => {:actors => [:shatner], :groups => []},
            :update => {:actors => [],         :groups => []}, # <--- That's the one!
            :delete => {:actors => [:shatner], :groups => []},
            :grant  => {:actors => [:shatner], :groups => []}
          }

          it "cannot add a user to the group" do
            put("/groups/#{hipsters}/actors/#{shatner}",
                :shatner).should have_status_code(403).
              with_body({"error" => "must be in the update access control entry to perform this action"})
            get("/groups/#{hipsters}", :superuser).should have_status_code(200).
              with_body({"actors" => [], "groups" => []})
          end
        end

        context "an actor indirectly in the UPDATE ACE" do
          with_actor :shatner
          with_group :hipsters
          with_group :brogrammers, :actors => [:shatner]

          with_ace_on :hipsters, :update, :groups => [:brogrammers]

          it "can add a user to the group" do
            put("/groups/#{hipsters}/actors/#{shatner}",
                :shatner).should have_status_code(200).with_body({})
            get("/groups/#{hipsters}", :superuser).should have_status_code(200).
              with_body({"actors" => [shatner], "groups" => []})
          end
        end

        context "with a non-existent target" do
          with_actor :shatner

          it "can't add a user to a group, because it doesn't exist" do
            fake_group = car_salesmen

            put("/groups/#{fake_group}/actors/#{shatner}",
                :shatner).should have_status_code(404)
          end
        end
      end # for actors

      context "for groups" do
        context "an actor directly in the UPDATE ACE" do
          with_actor :shatner
          with_group :hipsters
          with_group :brogrammers

          with_ace_on :hipsters, :update, :actors => [:shatner]

          it "can add a group to the group" do
            put("/groups/#{hipsters}/groups/#{brogrammers}",
                :shatner).should have_status_code(200).with_body({})
            get("/groups/#{hipsters}", :superuser).should have_status_code(200).
              with_body({"actors" => [], "groups" => [brogrammers]})
          end
        end

        # Because this has side effects, we have to have a new context so
        # that the group membership is properly (re-?)initialized
        context "an actor directly in the UPDATE ACE (2)" do
          with_actor :shatner
          with_group :hipsters
          with_group :brogrammers

          with_ace_on :hipsters, :update, :actors => [:shatner]

          it "cannot add a group to itself" do
            pending "you shouldn't be able to" do
              put("/groups/#{hipsters}/groups/#{hipsters}",
                  :shatner).should have_status_code(400).
                with_body({"error" => "cycles are bad, mmmkay"})
              get("/groups/#{hipsters}", :superuser).should have_status_code(200).
                with_body({"actors" => [], "groups" => []})
            end
          end
        end

        context "an actor directly in the UPDATE ACE (3)" do
          with_actor :shatner
          with_group :hipsters
          with_group :brogrammers

          with_ace_on :hipsters, :update, :actors => [:shatner]

          it "can add the same group to the group" do
            put("/groups/#{hipsters}/groups/#{brogrammers}",
                :shatner).should have_status_code(200).with_body({})
            get("/groups/#{hipsters}", :superuser).should have_status_code(200).
              with_body({"actors" => [], "groups" => [brogrammers]})

            put("/groups/#{hipsters}/groups/#{brogrammers}",
                :shatner).should have_status_code(200).with_body({})
            get("/groups/#{hipsters}", :superuser).should have_status_code(200).
              with_body({"actors" => [], "groups" => [brogrammers]})
          end
        end

        context "an actor directly in the UPDATE ACE (4)" do
          with_actor :shatner
          with_group :hipsters
          with_group :brogrammers

          with_ace_on :hipsters, :update, :actors => [:shatner]

          it "adding a bogus group raises an error" do
            bogus_group = "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz"

            put("/groups/#{hipsters}/groups/#{bogus_group}",
                :shatner).should have_status_code(403).
              with_body({"error" => "invalid group"})
            get("/groups/#{hipsters}", :superuser).should have_status_code(200).
              with_body({"actors" => [], "groups" => []})
          end

          it "adding a non-existent group raises an error" do
            fake_group = car_salesmen

            put("/groups/#{hipsters}/groups/#{fake_group}",
                :shatner).should have_status_code(403).
              with_body({"error" => "invalid group"})
            get("/groups/#{hipsters}", :superuser).should have_status_code(200).
              with_body({"actors" => [], "groups" => []})
          end
        end

        context "an actor NOT in the UPDATE ACE" do
          with_actor :shatner
          with_group :hipsters
          with_group :brogrammers

          # Give shatner everything EXCEPT update
          with_acl_on :hipsters, {
            :create => {:actors => [:shatner], :groups => []},
            :read   => {:actors => [:shatner], :groups => []},
            :update => {:actors => [],         :groups => []}, # <--- That's the one!
            :delete => {:actors => [:shatner], :groups => []},
            :grant  => {:actors => [:shatner], :groups => []}
          }

          it "cannot add a group to the group" do
            put("/groups/#{hipsters}/groups/#{brogrammers}",
                :shatner).should have_status_code(403).
              with_body({"error" => "must be in the update access control entry to perform this action"})
            get("/groups/#{hipsters}", :superuser).should have_status_code(200).
              with_body({"actors" => [], "groups" => []})
          end
        end

        context "an actor indirectly in the UPDATE ACE" do
          with_actor :shatner
          with_group :hipsters
          with_group :brogrammers, :actors => [:shatner]
          with_group :commies

          with_ace_on :hipsters, :update, :groups => [:brogrammers]

          it "can add a group to the group" do
            put("/groups/#{hipsters}/groups/#{commies}",
                :shatner).should have_status_code(200).with_body({})
            get("/groups/#{hipsters}", :superuser).should have_status_code(200).
              with_body({"actors" => [], "groups" => [commies]})
          end
        end

        context "group cycles" do
          with_actor :shatner
          with_group :hipsters
          with_group :brogrammers, :groups => [:hipsters]

          with_ace_on :hipsters, :update, :actors => [:shatner]

          it "are disallowed" do
            pending "they should be" do
              put("/groups/#{hipsters}/groups/#{brogrammers}",
                  :shatner).should have_status_code(400).
                with_body({"error" => "cycles are bad, mmmkay"})
              get("/groups/#{hipsters}", :superuser).should have_status_code(200).
                with_body({"actors" => [], "groups" => []})
            end
          end
        end

        context "deep group cycles" do
          # Just in case we make an implementation error or major
          # schema change that introduces errors that the above test
          # doesn't catch

          with_actor :shatner
          with_group :hipsters
          with_group :brogrammers, :groups => [:hipsters]
          with_group :commies, :groups => [:brogrammers]
          with_group :dirtycommies, :groups => [:commies]

          with_ace_on :hipsters, :update, :actors => [:shatner]

          it "are disallowed" do
            pending "they should be" do
              put("/groups/#{hipsters}/groups/#{dirtycommies}",
                  :shatner).should have_status_code(400).
                with_body({"error" => "cycles are bad, mmmkay"})
              get("/groups/#{hipsters}", :superuser).should have_status_code(200).
                with_body({"actors" => [], "groups" => []})
            end
          end
        end

        context "with a non-existent target" do
          with_actor :shatner
          with_group :hipsters

          it "can't add a user to a group, because it doesn't exist" do
            fake_group = car_salesmen

            put("/groups/#{fake_group}/groups/#{hipsters}",
                :shatner).should have_status_code(404)
          end
        end
      end # for groups
    end # PUT

    # Delete an actor / group from the group
    #
    # Members of actors and group lists can be removed.  Removing
    # members that aren't actually members fails with 404.  Ditto for
    # removing "members" that don't really exist.
    context "DELETE" do
      context "for actors" do
        context "an actor directly in the UPDATE ACE" do
          with_actor :shatner
          with_group :hipsters, :actors => [:shatner]

          with_ace_on :hipsters, :update, :actors => [:shatner]

          it "can delete a user from the group" do
            delete("/groups/#{hipsters}/actors/#{shatner}",
                :shatner).should have_status_code(200).with_body({})
            get("/groups/#{hipsters}", :superuser).should have_status_code(200).
              with_body({"actors" => [], "groups" => []})
          end
        end

        # Because this has side effects, we have to have a new context so
        # that the group membership is properly (re-?)initialized
        context "an actor directly in the UPDATE ACE (2)" do
          with_actor :shatner
          with_group :hipsters, :actors => [:shatner]

          with_ace_on :hipsters, :update, :actors => [:shatner]

          it "cannot delete a user from an empty group" do
            delete("/groups/#{hipsters}/actors/#{shatner}",
                :shatner).should have_status_code(200).with_body({})
            get("/groups/#{hipsters}", :superuser).should have_status_code(200).
              with_body({"actors" => [], "groups" => []})

            delete("/groups/#{hipsters}/actors/#{shatner}",
                :shatner).should have_status_code(404).
              with_body({"error" => "actor is not a member of the group"})
            get("/groups/#{hipsters}", :superuser).should have_status_code(200).
              with_body({"actors" => [], "groups" => []})
          end
        end

        context "an actor directly in the UPDATE ACE (3)" do
          with_actor :shatner
          with_group :hipsters, :actors => [:shatner]

          with_ace_on :hipsters, :update, :actors => [:shatner]

          it "deleting a bogus user raises an error" do
            bogus_actor = "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz"

            # TODO: maybe change the test?  Not sure if a we really
            # want to return 404s for bogus requests, though
            pending "returns 404 instead" do
              delete("/groups/#{hipsters}/actors/#{bogus_actor}",
                     :shatner).should have_status_code(403).
                with_body({"error" => "invalid actor"})
              get("/groups/#{hipsters}", :superuser).should have_status_code(200).
                with_body({"actors" => [shatner], "groups" => []})
            end
          end
        end

        context "an actor directly in the UPDATE ACE (4)" do
          with_actor :shatner
          with_group :hipsters, :actors => [:shatner]

          with_ace_on :hipsters, :update, :actors => [:shatner]

          it "deleting a non-existent user raises an error" do
            fake_actor = mattdamon

            pending "returns 404 instead" do
              delete("/groups/#{hipsters}/actors/#{fake_actor}",
                     :shatner).should have_status_code(403).
                with_body({"error" => "invalid actor"})
              get("/groups/#{hipsters}", :superuser).should have_status_code(200).
                with_body({"actors" => [shatner], "groups" => []})
            end
          end
        end

        context "an actor NOT in the UPDATE ACE" do
          with_actor :shatner
          with_group :hipsters, :actors => [:shatner]

          # Give shatner everything EXCEPT update
          with_acl_on :hipsters, {
            :create => {:actors => [:shatner], :groups => []},
            :read   => {:actors => [:shatner], :groups => []},
            :update => {:actors => [],         :groups => []}, # <--- That's the one!
            :delete => {:actors => [:shatner], :groups => []},
            :grant  => {:actors => [:shatner], :groups => []}
          }

          it "cannot delete a user from the group" do
            delete("/groups/#{hipsters}/actors/#{shatner}",
                :shatner).should have_status_code(403).
              with_body({"error" => "must be in the update access control entry to perform this action"})
            get("/groups/#{hipsters}", :superuser).should have_status_code(200).
              with_body({"actors" => [shatner], "groups" => []})
          end
        end

        context "an actor indirectly in the UPDATE ACE" do
          with_actor :shatner
          with_group :hipsters, :actors => [:shatner]
          with_group :brogrammers, :actors => [:shatner]

          with_ace_on :hipsters, :update, :groups => [:brogrammers]

          it "can delete a user from the group" do
            delete("/groups/#{hipsters}/actors/#{shatner}",
                :shatner).should have_status_code(200).with_body({})
            get("/groups/#{hipsters}", :superuser).should have_status_code(200).
              with_body({"actors" => [], "groups" => []})
          end
        end

        context "with a non-existent target" do
          with_actor :shatner

          it "can't delete a user from a group, because it doesn't exist" do
            fake_group = car_salesmen

            delete("/groups/#{fake_group}/actors/#{shatner}",
                   :shatner).should have_status_code(404)
          end
        end
      end # for actors

      context "for groups" do
        context "an actor directly in the UPDATE ACE" do
          with_actor :shatner
          with_group :brogrammers
          with_group :hipsters, :groups => [:brogrammers]

          with_ace_on :hipsters, :update, :actors => [:shatner]

          it "can delete a user from the group" do
            delete("/groups/#{hipsters}/groups/#{brogrammers}",
                :shatner).should have_status_code(200).with_body({})
            get("/groups/#{hipsters}", :superuser).should have_status_code(200).
              with_body({"actors" => [], "groups" => []})
          end
        end

        # Because this has side effects, we have to have a new context
        # so that the group membership is properly (re-?)initialized
        context "an actor directly in the UPDATE ACE (2)" do
          with_actor :shatner
          with_group :brogrammers
          with_group :hipsters, :groups => [:brogrammers]

          with_ace_on :hipsters, :update, :actors => [:shatner]

          it "cannot delete a user from an empty group" do
            delete("/groups/#{hipsters}/groups/#{brogrammers}",
                :shatner).should have_status_code(200).with_body({})
            get("/groups/#{hipsters}", :superuser).should have_status_code(200).
              with_body({"actors" => [], "groups" => []})

            delete("/groups/#{hipsters}/groups/#{brogrammers}",
                :shatner).should have_status_code(404).
              with_body({"error" => "group is not a member of the group"})
            get("/groups/#{hipsters}", :superuser).should have_status_code(200).
              with_body({"actors" => [], "groups" => []})
          end
        end

        context "an actor directly in the UPDATE ACE (3)" do
          with_actor :shatner
          with_group :brogrammers
          with_group :hipsters, :groups => [:brogrammers]

          with_ace_on :hipsters, :update, :actors => [:shatner]

          it "deleting a bogus user raises an error" do
            bogus_group = "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz"

            # TODO: maybe change the test?  Not sure if a we really
            # want to return 404s for bogus requests, though
            pending "returns 404 instead" do
              delete("/groups/#{hipsters}/groups/#{bogus_group}",
                     :shatner).should have_status_code(403).
                with_body({"error" => "invalid group"})
              get("/groups/#{hipsters}", :superuser).should have_status_code(200).
                with_body({"actors" => [], "groups" => [brogrammers]})
            end
          end
        end

        context "an actor directly in the UPDATE ACE (4)" do
          with_actor :shatner
          with_group :brogrammers
          with_group :hipsters, :groups => [:brogrammers]

          with_ace_on :hipsters, :update, :actors => [:shatner]

          it "deleting a non-existent user raises an error" do
            fake_group = car_salesmen

            pending "returns 404 instead" do
              delete("/groups/#{hipsters}/groups/#{fake_group}",
                     :shatner).should have_status_code(403).
                with_body({"error" => "invalid group"})
              get("/groups/#{hipsters}", :superuser).should have_status_code(200).
                with_body({"actors" => [], "groups" => [brogrammers]})
            end
          end
        end

        context "an actor NOT in the UPDATE ACE" do
          with_actor :shatner
          with_group :brogrammers
          with_group :hipsters, :groups => [:brogrammers]

          # Give shatner everything EXCEPT update
          with_acl_on :hipsters, {
            :create => {:actors => [:shatner], :groups => []},
            :read   => {:actors => [:shatner], :groups => []},
            :update => {:actors => [],         :groups => []}, # <--- That's the one!
            :delete => {:actors => [:shatner], :groups => []},
            :grant  => {:actors => [:shatner], :groups => []}
          }

          it "cannot delete a user from the group" do
            delete("/groups/#{hipsters}/groups/#{brogrammers}",
                :shatner).should have_status_code(403).
              with_body({"error" => "must be in the update access control entry to perform this action"})
            get("/groups/#{hipsters}", :superuser).should have_status_code(200).
              with_body({"actors" => [], "groups" => [brogrammers]})
          end
        end

        context "an actor indirectly in the UPDATE ACE" do
          with_actor :shatner
          with_group :dirtycommies
          with_group :hipsters, :groups => [:dirtycommies]
          with_group :brogrammers, :actors => [:shatner]

          with_ace_on :hipsters, :update, :groups => [:brogrammers]

          it "can delete a user from the group" do
            delete("/groups/#{hipsters}/groups/#{dirtycommies}",
                :shatner).should have_status_code(200).with_body({})
            get("/groups/#{hipsters}", :superuser).should have_status_code(200).
              with_body({"actors" => [], "groups" => []})
          end
        end

        context "with a non-existent target" do
          with_actor :shatner
          with_group :hipsters

          it "can't delete a user from a group, because it doesn't exist" do
            fake_group = car_salesmen

            delete("/groups/#{fake_group}/groups/#{hipsters}",
                   :shatner).should have_status_code(404)
          end
        end
      end # for groups
    end # DELETE
  end # /groups/<group_id>/<member_type>/<member_id>
end
