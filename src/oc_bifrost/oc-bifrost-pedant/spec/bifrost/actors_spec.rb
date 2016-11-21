describe "Actors Endpoint" do
  let(:mattdamon) { "deadbeefdeadbeefdeadbeefdeadbeef" }

  context "/actors" do
    # What we are testing:

    # Here we test actor creation (all other HTTP verbs should be
    # disallowed), making sure the response body is correct and that
    # id and id in the uri match, as well as basic header validation,
    # as well as making sure that the correct ACLs are created both
    # for the actor itself (i.e., an actor should default to having
    # permissions on themselves) and the requesting actor (i.e., the
    # requesting actor should have permissions on the new actor)

    should_not_allow :GET, "/actors"

    # POST creates a new actor
    #
    # NOTE: the return value for this a {"id": ID, "uri": URI} hash
    context "POST" do

      # We mainly do this to make sure the test cleans up after
      # itself; otherwise we have to repeat the hacky after :each with
      # the @actor_id stuff, and, well this is pretty much the same
      # for every creation
      def self.creates_actor_as(requestor, headers = {})
        after :each do
          delete("/actors/#{@actor_id}", :superuser)
        end

        it "creates an actor" do
          response = post("/actors", requestor, headers)

          # TODO: de-hardcode uri hostname in response body, make configurable
          response.should have_status_code(201).
                           with_body({"id" => /^[0-9a-f]{32}$/, "uri" => /[0-9a-fhttp:\\]*/})
          # TODO URI: URI code broken
          #"uri" => /^#{Pedant.config[:host]}:#{Pedant.config[:port]}\/actors\/[0-9a-f]{32}$/})
       
          @actor_id = parse(response)["id"]

          # Verify that uri and id are the same
          uri_id = parse(response)["uri"].split("/")[-1]
          uri_id.should == @actor_id
        end
      end

      context "as a superuser" do
        creates_actor_as(:superuser)
      end

      # This is one of the actual changes in behavior between old authz and new V1
      # of Bifrost; this actually works with the old server, but it can't with the
      # new schema because it's not possible to put bogus ACLs in the database which
      # this would require
      context "as an unknown requestor" do
        let(:fake_actor) { mattdamon }

        it "should not create an actor" do
          response = post("/actors", fake_actor)

          response.should have_status_code(401).
            with_body({"error" => "requesting actor id of '#{fake_actor}' does not exist"})
        end
      end

      # Interestingly, this behaves differently than other
      # endpoints...  Apparently, this is the only item creation
      # operation that doesn't require this header; is this the
      # "correct" behavior?
      context "without the X-Ops-Requesting-Actor-Id header" do
        creates_actor_as(:superuser,
                         :merge_headers => {"X-Ops-Requesting-Actor-Id" => :DELETE})
      end

      # Not quite clear the purpose of this header, actually
      context "without the X-Ops-User-Id header" do
        creates_actor_as(:superuser,
                         :merge_headers => {"X-Ops-User-Id" => :DELETE})
      end

      # Yes, this is valid behavior according to the current Authz
      # (and again is different than other endpoints)
      context "without ANY of the standard headers except Content-Type" do
        creates_actor_as(:superuser,
                         :headers => {"Content-Type" => "application/json"})
      end

      context "without any headers" do
        it "should NOT create an actor" do
          # Unsupported media type (i.e, no JSON content type header) = 415
          post("/actors", :superuser, :headers => {}).should have_status_code(415)

          # This has been verified (the hard way) that it doesn't create an actor --
          # nothing shows up in the database.  It's impossible to test that here,
          # because there's no ID to test, we get a 415 from webmachine before it
          # really gets to any of our code -- we can't test every possible ID just
          # to make sure nothing new exists.
        end
      end

      context "created actor" do
        with_actor :shatner

        it "is in own ACEs" do
          :shatner.should directly_have_permission(:create).on(:actor, :shatner)
          :shatner.should directly_have_permission(:read).on(:actor, :shatner)
          :shatner.should directly_have_permission(:update).on(:actor, :shatner)
          :shatner.should directly_have_permission(:delete).on(:actor, :shatner)
          :shatner.should directly_have_permission(:grant).on(:actor, :shatner)
        end
      end

      context "created actor part deux" do
        with_actor :shatner

        before :each do
          response = post("/actors", shatner)
          @actor = parse(response)["id"]
        end

        after :each do
          delete("/actors/#{@actor}", shatner)
        end

        it "contains creator in ACLs" do
          body = {"create" => {"actors" => [@actor, shatner], "groups" => []},
            "read" => {"actors" => [@actor, shatner], "groups" => []},
            "update" => {"actors" => [@actor, shatner], "groups" => []},
            "delete" => {"actors" => [@actor, shatner], "groups" => []},
            "grant" => {"actors" => [@actor, shatner], "groups" => []}}
          
          get("/actors/#{@actor}/acl",
              :superuser).should have_status_code(200).with_body(body)
        end
      end
    end # POST

    should_not_allow :PUT, "/actors"
    should_not_allow :DELETE, "/actors"
  end # /actors

  context "/actors/<actor_id>" do
    # What we are testing:

    # Here we test actor existence with GET (should require
    # appropriate READ access), as well as the ability to delete
    # actors (should require appropriate DELETE access).  All other
    # HTTP verbs should be disallowed.

    # NOTE: This is borderline pointless, since it always returns an
    # empty JSON object!  It can basically only answer the question
    # "Is there an actor with this ID?"
    context "GET" do
      context "an actor directly in the READ ACE" do
        with_actors :hasselhoff, :shatner

        with_ace_on :shatner, :read, :to => :hasselhoff

        it "can read the actor" do
          get("/actors/#{shatner}",
              :hasselhoff).should have_status_code(200).with_body({})
        end
      end

      context "an actor NOT in the READ ACE" do
        with_actors :malkovich, :shatner

        # Give malkovich everything EXCEPT read
        with_acl_on :shatner, {
          :create => {:actors => [:malkovich], :groups => []},
          :read   => {:actors => [],           :groups => []}, # <--- That's the one!
          :update => {:actors => [:malkovich], :groups => []},
          :delete => {:actors => [:malkovich], :groups => []},
          :grant  => {:actors => [:malkovich], :groups => []}
        }

        it "cannot read the actor" do
          get("/actors/#{shatner}", :malkovich).should have_status_code(403).
            with_body({"error" => "must be in the read access control entry to perform this action"})
        end
      end

      context "an actor indirectly in the READ ACE" do
        with_actors :hasselhoff, :shatner
        with_group :hipsters, :members => [:hasselhoff]

        with_ace_on :shatner, :read, :to => :hipsters

        it "can read the actor" do
          get("/actors/#{shatner}",
              :hasselhoff).should have_status_code(200).with_body({})
        end
      end

      context "with a non-existent target" do
        with_actor :hasselhoff

        it "can't be read, because it doesn't exist" do
          fake_actor = mattdamon

          get("/actors/#{fake_actor}", :hasselhoff).should have_status_code(404)
        end
      end
    end # GET

    should_not_allow :POST, "/actors/ffffffffffffffffffffffffffffffff"
    should_not_allow :PUT, "/actors/ffffffffffffffffffffffffffffffff"

    # DELETE deletes the actor
    #
    # TODO: How does this affect ACLs the actor is a member of?  (With the new
    # schema, presumably the actor will be removed from all ACL and groups it
    # belongs to)
    context "DELETE" do

      context "an actor directly in the DELETE ACE" do
        with_actors :hasselhoff, :shatner

        with_ace_on :shatner, :delete, :to => :hasselhoff

        it "can delete the actor" do
          delete("/actors/#{shatner}",
                 :hasselhoff).should have_status_code(200).with_body({})
          get("/actors/#{shatner}", :superuser).should have_status_code(404)
        end
      end

      context "an actor NOT in the DELETE ACE" do
        with_actors :malkovich, :shatner

        # Give malkovich everything EXCEPT delete
        with_acl_on :shatner, {
          :create => {:actors => [:malkovich], :groups => []},
          :read   => {:actors => [:malkovich], :groups => []},
          :update => {:actors => [:malkovich], :groups => []},
          :delete => {:actors => [],           :groups => []}, # <--- That's the one!
          :grant  => {:actors => [:malkovich], :groups => []}
        }

        it "cannot delete the actor" do
          delete("/actors/#{shatner}", :malkovich).should have_status_code(403).
            with_body({"error" => "must be in the delete access control entry to perform this action"})
          get("/actors/#{shatner}", :superuser).should have_status_code(200)
        end
      end

      context "an actor indirectly in the DELETE ACE" do
        with_actors :hasselhoff, :shatner
        with_group :hipsters, :members => [:hasselhoff]

        with_ace_on :shatner, :delete, :to => :hipsters

        it "can delete the actor" do
          delete("/actors/#{shatner}",
                 :hasselhoff).should have_status_code(200).with_body({})
          get("/actors/#{shatner}", :superuser).should have_status_code(404)
        end
      end

      context "with a non-existent target" do
        with_actor :hasselhoff

        it "can't be deleted, because it doesn't exist" do
          fake_actor = mattdamon

          # Prove it doesn't exist
          get("/actors/#{fake_actor}", :hasselhoff).should have_status_code(404)

          # Now try to delete it
          delete("/actors/#{fake_actor}", :hasselhoff).should have_status_code(404)
        end
      end
    end # DELETE
  end # /actors/<actor_id>

  context "/actors/<actor_id>/acl" do
    # What we are testing:

    # Here we test access to actor's ACL and that the response body
    # has the correct format.  Apparently, any ACE at all grants
    # access to the ACL -- we test each ACE in turn, both directly and
    # indirectly through a group.  All other HTTP verbs should be
    # disallowed.

    # The actor ACL tests are distinct from the other ACL tests
    # because actors behave slightly differently than other types --
    # actors always contain themselves in their own ACLs when they are
    # created.  They also contain the actor that created them (except
    # the superuser), but since we do use the superuser to create the
    # actor when we invoke with_actors, we don't test this explicitly
    # here (for that test, see above).

    context "GET" do
      ['create', 'read', 'update', 'delete', 'grant'].each do |ace|

        context "an actor directly in the #{ace.upcase} ACE" do
          with_actors :hasselhoff, :shatner

          with_ace_on :shatner, ace.to_sym, :to => :hasselhoff

          it "can read the acl" do
            body = {
              "create" => {"actors" => [shatner], "groups" => []},
              "read" => {"actors" => [shatner], "groups" => []},
              "update" => {"actors" => [shatner], "groups" => []},
              "delete" => {"actors" => [shatner], "groups" => []},
              "grant" => {"actors" => [shatner], "groups" => []}
            }
            body[ace] = {"actors" => [hasselhoff], "groups" => []}

            get("/actors/#{shatner}/acl",
                :hasselhoff).should have_status_code(200).with_body(body)
          end
        end

        context "an actor indirectly in the #{ace.upcase} ACE" do
          with_actors :hasselhoff, :shatner
          with_group :hipsters, :members => [:hasselhoff]

          with_ace_on :shatner, ace.to_sym, :to => :hipsters

          it "can read the acl" do
            body = {
              "create" => {"actors" => [shatner], "groups" => []},
              "read" => {"actors" => [shatner], "groups" => []},
              "update" => {"actors" => [shatner], "groups" => []},
              "delete" => {"actors" => [shatner], "groups" => []},
              "grant" => {"actors" => [shatner], "groups" => []}
            }
            body[ace] = {"actors" => [], "groups" => [hipsters]}

            get("/actors/#{shatner}/acl",
                :hasselhoff).should have_status_code(200).with_body(body)
          end
        end
      end

      context "an actor with NO ACE" do
        with_actors :malkovich, :shatner

        # Give malkovich no access at all
        with_acl_on :shatner, {
          :create => {:actors => [], :groups => []},
          :read   => {:actors => [], :groups => []},
          :update => {:actors => [], :groups => []},
          :delete => {:actors => [], :groups => []},
          :grant  => {:actors => [], :groups => []}
        }

        it "cannot read the acl" do
          get("/actors/#{shatner}/acl", :malkovich).should have_status_code(403).
            with_body({"error" => "must be in one of the create, read, update, delete, grant access control entries to perform this action"})
        end
      end

      context "with a non-existent target" do
        with_actor :hasselhoff

        it "can't be read, because it doesn't exist" do
          fake_actor = mattdamon

          get("/actors/#{fake_actor}/acl", :hasselhoff).should have_status_code(404)
        end
      end
    end # GET

    # NOTE: We'll want to eventually allow these operations in order
    # to facilitate bulk operations
    should_not_allow :POST, "/actors/ffffffffffffffffffffffffffffffff/acl"
    should_not_allow :PUT, "/actors/ffffffffffffffffffffffffffffffff/acl"
    should_not_allow :DELETE, "/actors/ffffffffffffffffffffffffffffffff/acl"
  end # /actors/<actor_id>/acl

  context "/actors/<actor_id>/acl/<action>" do
    # What we are testing:

    # Here we test access to a specific ACE/action in actor's ACL and
    # that the response body has the correct format.  Apparently, any
    # ACE at all grants access to the ACL -- we test each ACE in turn,
    # both directly and indirectly through a group.  PUT is used for
    # updating the ACL and is likewise tested (although there is
    # currently no checking for request correctness, and authz will
    # crash on badly formatted requests).  DELETE is also tested,
    # however it seems to be broken.  HTTP POST should be disallowed.

    context "GET" do
      ['create', 'read', 'update', 'delete', 'grant'].each do |action|
        context "for #{action.upcase} action" do

          ['create', 'read', 'update', 'delete', 'grant'].each do |ace|
            context "an actor directly in the #{ace.upcase} ACE" do
              with_actors :hasselhoff, :shatner

              with_ace_on :shatner, ace.to_sym, :to => :hasselhoff

              if (action == ace)
                let(:body) { {"actors" => [hasselhoff], "groups" => []} }
              else
                let(:body) { {"actors" => [shatner], "groups" => []} }
              end

              it "can read the acl" do
                get("/actors/#{shatner}/acl/#{action}",
                    :hasselhoff).should have_status_code(200).with_body(body)
              end
            end

            context "an actor indirectly in the #{ace.upcase} ACE" do
              with_actors :hasselhoff, :shatner
              with_group :hipsters, :members => [:hasselhoff]

              with_ace_on :shatner, ace.to_sym, :to => :hipsters

              if (action == ace)
                let(:body) { {"actors" => [], "groups" => [hipsters]} }
              else
                let(:body) { {"actors" => [shatner], "groups" => []} }
              end

              it "can read the acl" do
                get("/actors/#{shatner}/acl/#{action}",
                    :hasselhoff).should have_status_code(200).with_body(body)
              end
            end

            context "an actor with NO ACE" do
              with_actors :malkovich, :shatner

              # Give malkovich no access at all
              with_acl_on :shatner, {
                :create => {:actors => [], :groups => []},
                :read   => {:actors => [], :groups => []},
                :update => {:actors => [], :groups => []},
                :delete => {:actors => [], :groups => []},
                :grant  => {:actors => [], :groups => []}
              }

              it "cannot read the acl" do
                get("/actors/#{shatner}/acl/#{action}",
                    :malkovich).should have_status_code(403).with_body({"error" => "must be in one of the create, read, update, delete, grant access control entries to perform this action"})
              end
            end

            context "with a non-existent target" do
              with_actor :hasselhoff

              it "can't be read, because it doesn't exist" do
                fake_actor = mattdamon

                get("/actors/#{fake_actor}/acl/#{action}",
                    :hasselhoff).should have_status_code(404)
              end
            end
          end
        end
      end
    end # GET

    ['create', 'read', 'update', 'delete', 'grant'].each do |action|
      context "for #{action.upcase} action" do
        should_not_allow :POST, "/actors/ffffffffffffffffffffffffffffffff/acl/#{action}"
      end
    end

    # PUT replaces an ACE atomically

    context "PUT" do
      ['create', 'read', 'update', 'delete', 'grant'].each do |action|
        context "for #{action.upcase} action" do

          # TODO: probably want to expand this with various types of bad input,
          # although at the moment pretty much anything at all will crash it
          context "an actor directly in the GRANT ACE, with bad input" do
            with_actors :hasselhoff, :shatner

            with_ace_on :shatner, :grant, :to => :hasselhoff

            it "returns 400" do
              put("/actors/#{shatner}/acl/#{action}",
                :hasselhoff, :payload => {}).
                should have_status_code(400).
                with_body({"error" => "invalid JSON in request body"})

              if (action == 'grant')
                response_body = {"actors" => [hasselhoff], "groups" => []}
              else
                response_body = {"actors" => [shatner], "groups" => []}
              end

              get("/actors/#{shatner}/acl/#{action}",
                :superuser).should have_status_code(200).
                with_body(response_body)
            end
          end

          # TODO: I'm not sure these are a problem or not; we may want to properly
          # error these out down the road.  Also not sure we should return 400 for
          # non-existent actors/groups, dunno what the right HTTP response code is
          # for that.
          context "an actor directly in the GRANT ACE, with invalid actor" do
            with_actors :hasselhoff, :shatner

            with_ace_on :shatner, :grant, :to => :hasselhoff

            it "returns 400" do
              put("/actors/#{shatner}/acl/#{action}",
                :hasselhoff,
                :payload => {"actors" => ["zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz"],
                  "groups" => []}).
                should have_status_code(400).
                with_body({"error" => "attempt to add non-existent actor 'zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz' to ACL"})

              if (action == 'grant')
                body = {"actors" => [hasselhoff], "groups" => []}
              else
                body = {"actors" => [shatner], "groups" => []}
              end

              get("/actors/#{shatner}/acl/#{action}",
                :superuser).should have_status_code(200).
                with_body(body)
            end
          end

          context "an actor directly in the GRANT ACE, with invalid group" do
            with_actors :hasselhoff, :shatner

            with_ace_on :shatner, :grant, :to => :hasselhoff

            it "returns 400" do
              put("/actors/#{shatner}/acl/#{action}",
                :hasselhoff, :payload => {"actors" => [],
                  "groups" => ["zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz"]}).
                should have_status_code(400).
                with_body({"error" => "attempt to add non-existent group 'zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz' to ACL"})

              if (action == 'grant')
                body = {"actors" => [hasselhoff], "groups" => []}
              else
                body = {"actors" => [shatner], "groups" => []}
              end

              get("/actors/#{shatner}/acl/#{action}",
                :superuser).should have_status_code(200).
                with_body(body)
            end
          end

          context "an actor directly in the GRANT ACE, with non-existent actor" do
            with_actors :hasselhoff, :shatner

            with_ace_on :shatner, :grant, :to => :hasselhoff

            it "returns 400" do
              put("/actors/#{shatner}/acl/#{action}",
                :hasselhoff,
                :payload => {"actors" => ["ffffffffffffffffffffffffffffffff"],
                  "groups" => []}).
                should have_status_code(400).
                with_body({"error" => "attempt to add non-existent actor 'ffffffffffffffffffffffffffffffff' to ACL"})

              if (action == 'grant')
                body = {"actors" => [hasselhoff], "groups" => []}
              else
                body = {"actors" => [shatner], "groups" => []}
              end

              get("/actors/#{shatner}/acl/#{action}",
                :superuser).should have_status_code(200).
                with_body(body)
            end
          end

          context "an actor directly in the GRANT ACE, with non-existent group" do
            with_actors :hasselhoff, :shatner

            with_ace_on :shatner, :grant, :to => :hasselhoff

            it "returns 400" do
              put("/actors/#{shatner}/acl/#{action}",
                :hasselhoff, :payload => {"actors" => [],
                  "groups" => ["ffffffffffffffffffffffffffffffff"]}).
                should have_status_code(400).
                with_body({"error" => "attempt to add non-existent group 'ffffffffffffffffffffffffffffffff' to ACL"})

              if (action == 'grant')
                body = {"actors" => [hasselhoff], "groups" => []}
              else
                body = {"actors" => [shatner], "groups" => []}
              end

              get("/actors/#{shatner}/acl/#{action}",
                :superuser).should have_status_code(200).with_body(body)
            end
          end

          context "an actor directly in the GRANT ACE, modifying actors" do
            with_actors :hasselhoff, :schwartzenegger, :shatner

            with_ace_on :shatner, :grant, :to => :hasselhoff

            it "can modify the ACE for actors" do
              put("/actors/#{shatner}/acl/#{action}",
                  :hasselhoff, :payload => {"actors" => [schwartzenegger],
                    "groups" => []}).
                should have_status_code(200).with_body({})

              get("/actors/#{shatner}/acl/#{action}",
                  :superuser).should have_status_code(200).
                with_body({"actors" => [schwartzenegger], "groups" => []})
            end
          end

          context "an actor directly in the GRANT ACE, modifying groups" do
            with_actors :hasselhoff, :shatner
            with_group :brogrammers

            with_ace_on :shatner, :grant, :to => :hasselhoff

            it "can modify the ACE for groups" do
              put("/actors/#{shatner}/acl/#{action}",
                  :hasselhoff, :payload => {"actors" => [], "groups" => [brogrammers]}).
                should have_status_code(200).with_body({})

              get("/actors/#{shatner}/acl/#{action}",
                  :superuser).should have_status_code(200).
                with_body({"actors" => [], "groups" => [brogrammers]})
            end
          end

          context "an actor NOT in the GRANT ACE" do
            with_actors :malkovich, :schwartzenegger, :shatner

            # Give malkovich everything EXCEPT grant
            with_acl_on :shatner, {
              :create => {:actors => [:malkovich], :groups => []},
              :read   => {:actors => [:malkovich], :groups => []},
              :update => {:actors => [:malkovich], :groups => []},
              :delete => {:actors => [:malkovich], :groups => []},
              :grant  => {:actors => [],           :groups => []} # <--- That's the one!
            }

            if (action == 'grant')
              let(:body) { {"actors" => [], "groups" => []} }
            else
              let(:body) { {"actors" => [malkovich], "groups" => []} }
            end

            it "cannot modify the ACE" do
              put("/actors/#{shatner}/acl/#{action}",
                  :malkovich, :payload => {"actors" => [schwartzenegger],
                    "groups" => []}).should have_status_code(403).
                with_body({"error" => "must be in the grant access control entry to perform this action"})

              get("/actors/#{shatner}/acl/#{action}",
                  :superuser).should have_status_code(200).with_body(body)
            end
          end

          context "an actor indirectly in the GRANT ACE, modifying actors" do
            with_actors :hasselhoff, :shatner, :norris
            with_group :hipsters, :members => [:hasselhoff]

            with_ace_on :shatner, :grant, :to => :hipsters

            it "can modify the ACE for actors" do
              put("/actors/#{shatner}/acl/#{action}",
                     :hasselhoff, :payload => {"actors" => [norris], "groups" => []}).
                should have_status_code(200)
              get("/actors/#{shatner}/acl/#{action}",
                  :superuser).should have_status_code(200).
                with_body({"actors" => [norris], "groups" => []})
            end
          end

          context "an actor indirectly in the GRANT ACE, modifying groups" do
            with_actors :hasselhoff, :shatner
            with_group :hipsters, :members => [:hasselhoff]
            with_group :brogrammers

            with_ace_on :shatner, :grant, :to => :hipsters

            it "can modify the ACE for groups" do
              put("/actors/#{shatner}/acl/#{action}",
                     :hasselhoff, :payload => {"actors" => [],
                    "groups" => [brogrammers]}).should have_status_code(200)

              get("/actors/#{shatner}/acl/#{action}",
                  :superuser).should have_status_code(200).
                with_body({"actors" => [], "groups" => [brogrammers]})
            end
          end

          context "with a non-existent target" do
            with_actor :hasselhoff

            it "can't modify its ACE, because it doesn't exist" do
              fake_actor = mattdamon

              # Prove it doesn't exist
              get("/actors/#{fake_actor}/acl/#{action}",
                  :hasselhoff).should have_status_code(404)

              # Now try to modify it
              put("/actors/#{fake_actor}/acl/#{action}",
                  :hasselhoff).should have_status_code(404)
            end
          end
        end
      end
    end # PUT

    # DELETE clears actors and groups from ACE
    context "DELETE" do
      ['create', 'read', 'update', 'delete', 'grant'].each do |action|
        context "for #{action.upcase} action" do

          context "an actor directly in the GRANT ACE" do
            with_actors :hasselhoff, :shatner

            with_ace_on :shatner, :grant, :to => :hasselhoff

            it "can clear the ACE" do
              delete("/actors/#{shatner}/acl/#{action}",
                :hasselhoff).should have_status_code(200).with_body({})
              get("/actors/#{shatner}/acl/#{action}",
                :superuser).should have_status_code(200).
                with_body({"actors" => [], "groups" => []})
            end
          end

          context "an actor NOT in the GRANT ACE" do
            with_actors :malkovich, :shatner

            # Give malkovich everything EXCEPT grant
            with_acl_on :shatner, {
              :create => {:actors => [:malkovich], :groups => []},
              :read   => {:actors => [:malkovich], :groups => []},
              :update => {:actors => [:malkovich], :groups => []},
              :delete => {:actors => [:malkovich], :groups => []},
              :grant  => {:actors => [],           :groups => []} # <--- That's the one!
            }

            it "cannot clear the ACE" do
              delete("/actors/#{shatner}/acl/#{action}",
                     :malkovich).should have_status_code(403).
                with_body({"error" => "must be in the grant access control entry to perform this action"})

              if (action == 'grant')
                response_body = {"actors" => [], "groups" => []}
              else
                response_body = {"actors" => [malkovich], "groups" => []}
              end

              get("/actors/#{shatner}/acl/#{action}",
                  :superuser).should have_status_code(200).with_body(response_body)
            end
          end

          context "an actor indirectly in the GRANT ACE" do
            with_actors :hasselhoff, :shatner
            with_group :hipsters, :members => [:hasselhoff]

            with_ace_on :shatner, :grant, :to => :hipsters

            it "can clear the ACE" do
              delete("/actors/#{shatner}/acl/#{action}",
                :hasselhoff).should have_status_code(200).with_body({})

              response_body = {"actors" => [], "groups" => []}

              get("/actors/#{shatner}/acl/#{action}",
                :superuser).should have_status_code(200).with_body(response_body)
            end
          end

          context "with a non-existent target" do
            with_actor :hasselhoff

            it "can't clear its ACE, because it doesn't exist" do
              fake_actor = mattdamon

              # Prove it doesn't exist
              get("/actors/#{fake_actor}/acl/#{action}",
                  :hasselhoff).should have_status_code(404)

              # Now try to delete it
              delete("/actors/#{fake_actor}/acl/#{action}",
                     :hasselhoff).should have_status_code(404)
            end
          end
        end
      end
    end # DELETE
  end # /actors/<actor_id>/acl/<action>
end
