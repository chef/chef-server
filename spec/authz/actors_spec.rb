require 'pedant/rspec/actors_util.rb'

describe "Actors Endpoint" do
  include Pedant::RSpec::ActorsUtil

  context "/actors" do
    let(:request_url){"/actors"}

    should_not_allow :GET, "/actors"

    # POST creates a new actor
    #
    # NOTE: the return value for this a {"id": ID, "uri": URI} hash
    context "POST" do
      include_context "create an actor as"

      context "as a superuser" do
        creates_an_actor_as(:superuser)
      end

      context "as an unknown requestor" do
        creates_an_actor_as("deadbeefdeadbeefdeadbeefdeadbeef")
      end

      # Apparently, this is the only item creation operation that
      # doesn't require this header; is this the "correct" behavior?
      context "without the X-Ops-Requesting-Actor-Id header" do
        creates_an_actor_as(:superuser,
                            :merge_headers => {"X-Ops-Requesting-Actor-Id" => :DELETE})
      end

      # Not quite clear the purpose of this header, actually
      context "without the X-Ops-User-Id header" do
        creates_an_actor_as(:superuser,
                            :merge_headers => {"X-Ops-User-Id" => :DELETE})
      end

      # Yes, this is valid behavior according to the current Authz
      context "without ANY of the standard headers except Content-Type" do
        creates_an_actor_as(:superuser,
                            :headers => {"Content-Type" => "application/json"})
      end

      context "without any headers" do
        it "should NOT create an actor" do
          pending "currently returns a 415 AND A NEW ACTOR!" do
            post("/actors",
                 :superuser,
                 :headers => {}).should have_status_code(400).
              with_body({"error" => "That ain't right"})
            # Obviously this is not the EXACT response that should come back...
          end
        end
      end

      context "created actor" do
        with_actor :testy

        it "is in own ACEs" do
          :testy.should directly_have_permission(:create).on_actor(:testy)
          :testy.should directly_have_permission(:read).on_actor(:testy)
          :testy.should directly_have_permission(:update).on_actor(:testy)
          :testy.should directly_have_permission(:delete).on_actor(:testy)
          :testy.should directly_have_permission(:grant).on_actor(:testy)
        end
      end

      context "created actor part deux" do
        with_actor :testy

        before :each do
          response = post("/actors", testy)
          @actor = parse(response)["id"]
        end

        after :each do
          delete("/actors/#{@actor}", testy)
        end

        it "contains creator in ACLs" do
          body = {"create" => {"actors" => [@actor, testy], "groups" => []},
            "read" => {"actors" => [@actor, testy], "groups" => []},
            "update" => {"actors" => [@actor, testy], "groups" => []},
            "delete" => {"actors" => [@actor, testy], "groups" => []},
            "grant" => {"actors" => [@actor, testy], "groups" => []}}
          
          get("/actors/#{@actor}/acl",
              :superuser).should have_status_code(200).with_body(body)
        end
      end
    end # POST

    should_not_allow :PUT, "/actors"
    should_not_allow :DELETE, "/actors"
  end # /actors

  context "/actors/<actor_id>" do
    # GET returns the actor
    #
    # NOTE: This is borderline pointless, since it always returns an
    # empty JSON object!  It can basically only answer the question
    # "Is there an actor with this ID?"
    context "GET" do
      context "an actor directly in the READ ACE" do
        with_actors :alice, :testy

        with_ace_on_actor :testy, :read, :actors => [:alice]

        it "can read the actor" do
          :alice.should directly_have_permission(:read).on_actor(:testy)
          get("/actors/#{testy}", :alice).should have_status_code(200).with_body({})
        end
      end

      context "an actor NOT in the READ ACE" do
        with_actors :bob, :testy

        # Give bob everything EXCEPT read
        with_acl_on_actor :testy, {
          :create => {:actors => [:bob], :groups => []},
          :read   => {:actors => [],     :groups => []}, # <--- That's the one!
          :update => {:actors => [:bob], :groups => []},
          :delete => {:actors => [:bob], :groups => []},
          :grant  => {:actors => [:bob], :groups => []}
        }

        it "cannot read the actor" do
          :bob.should_not directly_have_permission(:read).on_actor(:testy)
          get("/actors/#{testy}",
              :bob).should have_status_code(403).with_body({"error" => "must be in the read access control entry to perform this action"})
        end
      end

      context "an actor inderectly in the READ ACE" do
        with_actors :alice, :testy, :bob
        with_group :hackers

        with_ace_on_actor :testy, :read, :groups => [:hackers]
        with_members :hackers, :actors => [:alice]

        it "can read the actor" do

          :alice.should_not directly_have_permission(:read).on_actor(:testy)
          :alice.should be_a_direct_member_of(:hackers)
          :hackers.should directly_have_permission(:read).on_actor(:testy)

          get("/actors/#{testy}", :alice).should have_status_code(200).with_body({})
        end
      end

      context "with a non-existent target" do
        with_actor :alice

        it "can't be read, because it doesn't exist" do
          fake_actor = "deadbeefdeadbeefdeadbeefdeadbeef"

          get("/actors/#{fake_actor}", :alice).should have_status_code(404)
        end
      end
    end # GET

    should_not_allow :POST, "/actors/ffffffffffffffffffffffffffffffff"
    should_not_allow :PUT, "/actors/ffffffffffffffffffffffffffffffff"

    # DELETE deletes the actor
    #
    # TODO: How does this affect ACLs the actor is a member of?
    context "DELETE" do

      context "an actor directly in the DELETE ACE" do
        with_actors :alice, :testy

        with_ace_on_actor :testy, :delete, :actors => [:alice]

        it "can delete the actor" do
          :alice.should directly_have_permission(:delete).on_actor(:testy)
          delete("/actors/#{testy}", :alice).should have_status_code(200).with_body({})
          get("/actors/#{testy}", :superuser).should have_status_code(404)
        end
      end

      context "an actor NOT in the DELETE ACE" do
        with_actors :bob, :testy

        # Give bob everything EXCEPT delete
        with_acl_on_actor :testy, {
          :create => {:actors => [:bob], :groups => []},
          :read   => {:actors => [:bob], :groups => []},
          :update => {:actors => [:bob], :groups => []},
          :delete => {:actors => [],     :groups => []}, # <--- That's the one!
          :grant  => {:actors => [:bob], :groups => []}
        }

        it "cannot delete the actor" do
          :bob.should_not directly_have_permission(:delete).on_actor(:testy)
          delete("/actors/#{testy}",
                 :bob).should have_status_code(403).with_body({"error" => "must be in the delete access control entry to perform this action"})
          get("/actors/#{testy}", :superuser).should have_status_code(200)
        end
      end

      context "an actor indirectly in the DELETE ACE" do
        with_actors :alice, :testy, :bob
        with_group :hackers

        with_ace_on_actor :testy, :delete, :groups => [:hackers]
        with_members :hackers, :actors => [:alice]

        it "can delete the actor" do

          :alice.should_not directly_have_permission(:delete).on_actor(:testy)
          :alice.should be_a_direct_member_of(:hackers)
          :hackers.should directly_have_permission(:delete).on_actor(:testy)

          delete("/actors/#{testy}", :alice).should have_status_code(200).with_body({})
          get("/actors/#{testy}", :superuser).should have_status_code(404)
        end
      end

      context "with a non-existent target" do
        with_actor :alice

        it "can't be deleted, because it doesn't exist" do
          fake_actor = "deadbeefdeadbeefdeadbeefdeadbeef"

          # Prove it doesn't exist
          get("/actors/#{fake_actor}", :alice).should have_status_code(404)

          # Now try to delete it
          delete("/actors/#{fake_actor}", :alice).should have_status_code(404)
        end
      end
    end # DELETE
  end # /actors/<actor_id>

  context "/actors/<actor_id>/acl" do
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

    context "GET" do
      include_context "create acl body"

      ['CREATE', 'READ', 'UPDATE', 'DELETE', 'GRANT'].each do |ace|

        context "an actor directly in the #{ace} ACE" do
          with_actors :alice, :testy

          with_ace_on_actor :testy, ace.downcase.to_sym, :actors => [:alice]

          let(:body) { acl_body_for_actor(testy, ace.downcase, alice) }

          it "can read the acl" do
            :alice.should directly_have_permission(ace.downcase.to_sym).
              on_actor(:testy)
            get("/actors/#{testy}/acl",
                :alice).should have_status_code(200).with_body(body)
          end
        end

        context "an actor inderectly in the #{ace} ACE" do
          with_actors :alice, :testy, :bob
          with_group :hackers

          with_ace_on_actor :testy, ace.downcase.to_sym, :groups => [:hackers]
          with_members :hackers, :actors => [:alice]

          let(:body) { acl_body_for_actor(testy, ace.downcase, nil, hackers) }

          it "can read the acl" do
            :alice.should_not directly_have_permission(ace.downcase.to_sym).
              on_actor(:testy)
            :alice.should be_a_direct_member_of(:hackers)
            :hackers.should directly_have_permission(ace.downcase.to_sym).
              on_actor(:testy)

            get("/actors/#{testy}/acl",
                :alice).should have_status_code(200).with_body(body)
          end
        end
      end

      context "an actor with NO ACE" do
        with_actors :bob, :testy

        # Give bob no access at all
        with_acl_on_actor :testy, {
          :create => {:actors => [], :groups => []},
          :read   => {:actors => [], :groups => []},
          :update => {:actors => [], :groups => []},
          :delete => {:actors => [], :groups => []},
          :grant  => {:actors => [], :groups => []}
        }

        it "cannot read the acl" do
          :bob.should_not directly_have_permission(:create).on_actor(:testy) 
          :bob.should_not directly_have_permission(:read).on_actor(:testy)
          :bob.should_not directly_have_permission(:update).on_actor(:testy)
          :bob.should_not directly_have_permission(:delete).on_actor(:testy)
          :bob.should_not directly_have_permission(:grant).on_actor(:testy)
         get("/actors/#{testy}/acl",
             :bob).should have_status_code(403).with_body({"error" => "must be in one of the create, read, update, delete, grant access control entries to perform this action"})
        end
      end

      context "with a non-existent target" do
        with_actor :alice

        it "can't be read, because it doesn't exist" do
          fake_actor = "deadbeefdeadbeefdeadbeefdeadbeef"

          get("/actors/#{fake_actor}/acl", :alice).should have_status_code(404)
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
    # GET actors and groups for action
    #
    # Cucumber: a newly-created actor should itself be present in each
    # of its own ACEs
    #
    # Cucumber: Additionally, a newly-created actor should contain
    # *the actor that created it* in each of its ACEs
    context "GET" do
      ['CREATE', 'READ', 'UPDATE', 'DELETE', 'GRANT'].each do |action|
        context "for #{action} action" do

          ['CREATE', 'READ', 'UPDATE', 'DELETE', 'GRANT'].each do |ace|

            context "an actor directly in the #{ace} ACE" do
              with_actors :alice, :testy

              with_ace_on_actor :testy, ace.downcase.to_sym, :actors => [:alice]

              if (action == ace)
                let(:body) { {"actors" => [alice], "groups" => []} }
              else
                let(:body) { {"actors" => [testy], "groups" => []} }
              end

              it "can read the acl" do
                :alice.should directly_have_permission(ace.downcase.to_sym).
                  on_actor(:testy)
                get("/actors/#{testy}/acl/#{action}",
                    :alice).should have_status_code(200).with_body(body)
              end
            end

            context "an actor inderectly in the #{ace} ACE" do
              with_actors :alice, :testy, :bob
              with_group :hackers

              with_ace_on_actor :testy, ace.downcase.to_sym, :groups => [:hackers]
              with_members :hackers, :actors => [:alice]

              if (action == ace)
                let(:body) { {"actors" => [], "groups" => [hackers]} }
              else
                let(:body) { {"actors" => [testy], "groups" => []} }
              end

              it "can read the acl" do
                :alice.should_not directly_have_permission(ace.downcase.to_sym).
                  on_actor(:testy)
                :alice.should be_a_direct_member_of(:hackers)
                :hackers.should directly_have_permission(ace.downcase.to_sym).
                  on_actor(:testy)

                get("/actors/#{testy}/acl/#{action}",
                    :alice).should have_status_code(200).with_body(body)
              end
            end

            context "an actor with NO ACE" do
              with_actors :bob, :testy

              # Give bob no access at all
              with_acl_on_actor :testy, {
                :create => {:actors => [], :groups => []},
                :read   => {:actors => [], :groups => []},
                :update => {:actors => [], :groups => []},
                :delete => {:actors => [], :groups => []},
                :grant  => {:actors => [], :groups => []}
              }

              it "cannot read the acl" do
                :bob.should_not directly_have_permission(:create).on_actor(:testy) 
                :bob.should_not directly_have_permission(:read).on_actor(:testy)
                :bob.should_not directly_have_permission(:update).on_actor(:testy)
                :bob.should_not directly_have_permission(:delete).on_actor(:testy)
                :bob.should_not directly_have_permission(:grant).on_actor(:testy)
                get("/actors/#{testy}/acl/#{action}",
                    :bob).should have_status_code(403).with_body({"error" => "must be in one of the create, read, update, delete, grant access control entries to perform this action"})
              end
            end

            context "with a non-existent target" do
              with_actor :alice

              it "can't be read, because it doesn't exist" do
                fake_actor = "deadbeefdeadbeefdeadbeefdeadbeef"

                get("/actors/#{fake_actor}/acl/#{action}",
                    :alice).should have_status_code(404)
              end
            end
          end
        end
      end
    end # GET

    should_not_allow :POST, "/actors/ffffffffffffffffffffffffffffffff/acl/create"

    # PUT replaces an ACE atomically

    context "PUT" do
      ['CREATE', 'READ', 'UPDATE', 'DELETE', 'GRANT'].each do |action|
        context "for #{action} action" do

          # TODO: probably want to expand this with various types of bad input,
          # although at the moment pretty much anything at all will crash it
          context "an actor directly in the GRANT ACE, with bad input" do
            with_actors :alice, :testy

            with_ace_on_actor :testy, :grant, :actors => [:alice]

            it "returns 400" do
              pending "returns 500 instead" do
                :alice.should directly_have_permission(:grant).on_actor(:testy)
                put("/actors/#{testy}/acl/#{action.downcase}",
                    :alice, :payload => {}).
                  should have_status_code(400).with_body({"error" => "bad input"})
                get("/actors/#{testy}/acl/#{action.downcase}",
                    :superuser).should have_status_code(200).
                  with_body({"actors" => [alice], "groups" => []})
              end
            end
          end

          # TODO: I'm not sure these are a problem or not; we may want to properly
          # error these out down the road.  Also not sure we should return 400 for
          # non-existent actors/groups, dunno what the right HTTP response code is
          # for that.

          context "an actor directly in the GRANT ACE, with invalid actor" do
            with_actors :alice, :testy

            with_ace_on_actor :testy, :grant, :actors => [:alice]

            it "returns 400" do
              pending "returns 200 instead" do
                :alice.should directly_have_permission(:grant).on_actor(:testy)
                put("/actors/#{testy}/acl/#{action.downcase}",
                    :alice,
                    :payload => {"actors" => ["zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz"],
                      "groups" => []}).
                  should have_status_code(400).with_body({"error" => "bad input"})
                get("/actors/#{testy}/acl/#{action.downcase}",
                    :superuser).should have_status_code(200).
                  with_body({"actors" => [alice], "groups" => []})
              end
            end
          end

          context "an actor directly in the GRANT ACE, with invalid group" do
            with_actors :alice, :testy

            with_ace_on_actor :testy, :grant, :actors => [:alice]

            it "returns 400" do
              pending "returns 200 instead" do
                :alice.should directly_have_permission(:grant).on_actor(:testy)
                put("/actors/#{testy}/acl/#{action.downcase}",
                    :alice, :payload => {"actors" => [],
                      "groups" => ["zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz"]}).
                  should have_status_code(400).with_body({"error" => "bad input"})
                get("/actors/#{testy}/acl/#{action.downcase}",
                    :superuser).should have_status_code(200).
                  with_body({"actors" => [alice], "groups" => []})
              end
            end
          end

          context "an actor directly in the GRANT ACE, with non-existent actor" do
            with_actors :alice, :testy

            with_ace_on_actor :testy, :grant, :actors => [:alice]

            it "returns 400" do
              pending "returns 200 instead" do
                :alice.should directly_have_permission(:grant).on_actor(:testy)
                put("/actors/#{testy}/acl/#{action.downcase}",
                    :alice,
                    :payload => {"actors" => ["ffffffffffffffffffffffffffffffff"],
                      "groups" => []}).
                  should have_status_code(400).with_body({"error" => "bad input"})
                get("/actors/#{testy}/acl/#{action.downcase}",
                    :superuser).should have_status_code(200).
                  with_body({"actors" => [alice], "groups" => []})
              end
            end
          end

          context "an actor directly in the GRANT ACE, with non-existent group" do
            with_actors :alice, :testy

            with_ace_on_actor :testy, :grant, :actors => [:alice]

            it "returns 400" do
              pending "returns 200 instead" do
                :alice.should directly_have_permission(:grant).on_actor(:testy)
                put("/actors/#{testy}/acl/#{action.downcase}",
                    :alice, :payload => {"actors" => [],
                      "groups" => ["ffffffffffffffffffffffffffffffff"]}).
                  should have_status_code(400).with_body({"error" => "bad input"})
                get("/actors/#{testy}/acl/#{action.downcase}",
                    :superuser).should have_status_code(200).
                  with_body({"actors" => [alice], "groups" => []})
              end
            end
          end

          context "an actor directly in the GRANT ACE, modifying actors" do
            with_actors :alice, :rainbowdash, :testy

            with_ace_on_actor :testy, :grant, :actors => [:alice]

            it "can modify the ACE for actors" do
              :alice.should directly_have_permission(:grant).on_actor(:testy)
              put("/actors/#{testy}/acl/#{action.downcase}",
                  :alice, :payload => {"actors" => [rainbowdash], "groups" => []}).
                should have_status_code(200).with_body({})
              get("/actors/#{testy}/acl/#{action.downcase}",
                  :superuser).should have_status_code(200).
                with_body({"actors" => [rainbowdash], "groups" => []})
            end
          end

          context "an actor directly in the GRANT ACE, modifying groups" do
            with_actors :alice, :testy
            with_group :ponies

            with_ace_on_actor :testy, :grant, :actors => [:alice]

            it "can modify the ACE for groups" do
              :alice.should directly_have_permission(:grant).on_actor(:testy)
              put("/actors/#{testy}/acl/#{action.downcase}",
                  :alice, :payload => {"actors" => [], "groups" => [ponies]}).
                should have_status_code(200).with_body({})
              get("/actors/#{testy}/acl/#{action.downcase}",
                  :superuser).should have_status_code(200).
                with_body({"actors" => [], "groups" => [ponies]})
            end
          end

          context "an actor NOT in the GRANT ACE" do
            with_actors :bob, :rainbowdash, :testy

            # Give bob everything EXCEPT grant
            with_acl_on_actor :testy, {
              :create => {:actors => [:bob], :groups => []},
              :read   => {:actors => [:bob], :groups => []},
              :update => {:actors => [:bob], :groups => []},
              :delete => {:actors => [:bob], :groups => []},
              :grant  => {:actors => [],     :groups => []} # <--- That's the one!
            }

            if (action == 'GRANT')
              let(:body) { {"actors" => [], "groups" => []} }
            else
              let(:body) { {"actors" => [bob], "groups" => []} }
            end

            it "cannot modify the ACE" do
              :bob.should_not directly_have_permission(:grant).on_actor(:testy)
              put("/actors/#{testy}/acl/#{action.downcase}",
                  :bob, :payload => {"actors" => [rainbowdash], "groups" => []}).
                should have_status_code(403).with_body({"error" => "must be in the grant access control entry to perform this action"})
              get("/actors/#{testy}/acl/#{action.downcase}",
                  :superuser).should have_status_code(200).with_body(body)
            end
          end

          context "an actor indirectly in the GRANT ACE, modifying actors" do
            with_actors :alice, :testy, :bob, :sparklepony
            with_groups :hackers

            with_ace_on_actor :testy, :grant, :groups => [:hackers]
            with_members :hackers, :actors => [:alice]

            it "can modify the ACE for actors" do
              :alice.should_not directly_have_permission(:grant).on_actor(:testy)
              :alice.should be_a_direct_member_of(:hackers)
              :hackers.should directly_have_permission(:grant).on_actor(:testy)

              put("/actors/#{testy}/acl/#{action.downcase}",
                     :alice, :payload => {"actors" => [sparklepony], "groups" => []}).
                should have_status_code(200)
              get("/actors/#{testy}/acl/#{action.downcase}",
                  :superuser).should have_status_code(200).
                with_body({"actors" => [sparklepony], "groups" => []})
            end
          end

          context "an actor indirectly in the GRANT ACE, modifying groups" do
            with_actors :alice, :testy, :bob
            with_groups :hackers, :ponies

            with_ace_on_actor :testy, :grant, :groups => [:hackers]
            with_members :hackers, :actors => [:alice]

            it "can modify the ACE for groups" do
              :alice.should_not directly_have_permission(:grant).on_actor(:testy)
              :alice.should be_a_direct_member_of(:hackers)
              :hackers.should directly_have_permission(:grant).on_actor(:testy)

              put("/actors/#{testy}/acl/#{action.downcase}",
                     :alice, :payload => {"actors" => [], "groups" => [ponies]}).
                should have_status_code(200)
              get("/actors/#{testy}/acl/#{action.downcase}",
                  :superuser).should have_status_code(200).
                with_body({"actors" => [], "groups" => [ponies]})
            end
          end

          context "with a non-existent target" do
            with_actor :alice

            it "can't modify its ACE, because it doesn't exist" do
              fake_actor = "deadbeefdeadbeefdeadbeefdeadbeef"

              # Prove it doesn't exist
              get("/actors/#{fake_actor}/acl/#{action.downcase}", :alice).should have_status_code(404)

              # Now try to delete it
              delete("/actors/#{fake_actor}/acl/#{action.downcase}", :alice).should have_status_code(404)
            end
          end
        end
      end
    end # PUT

    # DELETE clears actors and groups from ACE
    context "DELETE" do
      ['CREATE', 'READ', 'UPDATE', 'DELETE', 'GRANT'].each do |action|
        context "for #{action} action" do

          context "an actor directly in the GRANT ACE" do
            with_actors :alice, :testy

            with_ace_on_actor :testy, :grant, :actors => [:alice]

            it "can clear the ACE" do
              pending "causes internal 500 errors" do
                :alice.should directly_have_permission(:grant).on_actor(:testy)
                delete("/actors/#{testy}/acl/#{action.downcase}",
                       :alice).should have_status_code(200).with_body({})
                get("/actors/#{testy}/acl/#{action.downcase}",
                    :superuser).should have_status_code(404)
              end
            end
          end

          context "an actor NOT in the GRANT ACE" do
            with_actors :bob, :testy

            # Give bob everything EXCEPT grant
            with_acl_on_actor :testy, {
              :create => {:actors => [:bob], :groups => []},
              :read   => {:actors => [:bob], :groups => []},
              :update => {:actors => [:bob], :groups => []},
              :delete => {:actors => [:bob], :groups => []},
              :grant  => {:actors => [],     :groups => []} # <--- That's the one!
            }

            it "cannot clear the ACE" do
              :bob.should_not directly_have_permission(:grant).on_actor(:testy)
              delete("/actors/#{testy}/acl/#{action.downcase}",
                     :bob).should have_status_code(403).with_body({"error" => "must be in the grant access control entry to perform this action"})
              get("/actors/#{testy}/acl/#{action.downcase}",
                  :superuser).should have_status_code(200)
            end
          end

          context "an actor indirectly in the GRANT ACE" do
            with_actors :alice, :testy, :bob
            with_group :hackers

            with_ace_on_actor :testy, :grant, :groups => [:hackers]
            with_members :hackers, :actors => [:alice]

            it "can clear the ACE" do
              pending "causes internal 500 errors" do
                :alice.should_not directly_have_permission(:grant).on_actor(:testy)
                :alice.should be_a_direct_member_of(:hackers)
                :hackers.should directly_have_permission(:grant).on_actor(:testy)

                delete("/actors/#{testy}/acl/#{action.downcase}",
                       :alice).should have_status_code(200).with_body({})
                get("/actors/#{testy}/acl/#{action.downcase}",
                    :superuser).should have_status_code(404)
              end
            end
          end

          context "with a non-existent target" do
            with_actor :alice

            it "can't clear its ACE, because it doesn't exist" do
              fake_actor = "deadbeefdeadbeefdeadbeefdeadbeef"

              # Prove it doesn't exist
              get("/actors/#{fake_actor}/acl/#{action.downcase}", :alice).should have_status_code(404)

              # Now try to delete it
              delete("/actors/#{fake_actor}/acl/#{action.downcase}", :alice).should have_status_code(404)
            end
          end
        end
      end
    end # DELETE
  end # /actors/<actor_id>/acl/<action>

  context "/actors/<actor_id>/acl/<action>/<member_type>" do
    # These are basically null tests; every one of these DO have subpaths to
    # test, but certainly for smoke testing this is excessive.

    # Might want to cut some of these out -- containers and object
    # versions should always be 404 even when an ID is specified,
    # since they can't have permissions

    ['CREATE', 'READ', 'UPDATE', 'DELETE', 'GRANT'].each do |action|
      context "for #{action} action" do
        ['ACTORS', 'GROUPS', 'OBJECTS', 'CONTAINERS'].each do |type|
          context "for #{type} member type" do
            with_actor :testy

            it "get should not be found" do
              get("/actors/#{testy}/acl/#{action.downcase}/#{type.downcase}/",
                  :superuser).should have_status_code(404)
            end

            it "post should not be found" do
              post("/actors/#{testy}/acl/#{action.downcase}/#{type.downcase}/",
                   :superuser).should have_status_code(404)
            end

            it "put should not be found" do
              put("/actors/#{testy}/acl/#{action.downcase}/#{type.downcase}/",
                  :superuser).should have_status_code(404)
            end

            it "delete should not be found" do
              delete("/actors/#{testy}/acl/#{action.downcase}/#{type.downcase}/",
                     :superuser).should have_status_code(404)
            end
          end
        end
      end
    end
  end # /actors/<actor_id>/acl/<action>/<member_type>

  context "/actors/<actor_id>/acl/<action>/<member_type>/<member_id>" do

    # GET uses is_authorized_on_object to determine whether the
    # specified actor / group has the specified permission
    #
    # Returns a 200 and an empty JSON hash if the actor / group has
    # the permission
    #
    # TODO: Perhaps use 204 (OK, No Content) instead?
    context "GET" do
      ['CREATE', 'READ', 'UPDATE', 'DELETE', 'GRANT'].each do |action|
        context "for #{action} action" do
          ['CREATE', 'READ', 'UPDATE', 'DELETE', 'GRANT'].each do |ace|
            context "for ACTORS member type" do
              context "an actor directly in the #{ace} ACE" do
                with_actors :alice, :testy

                with_ace_on_actor :testy, ace.downcase.to_sym, :actors => [:alice]

                if (action == ace)
                  it "returns 200 when in ACE" do
                    :alice.should directly_have_permission(ace.downcase.to_sym).
                      on_actor(:testy)
                    # Alice has specific ACE access on testy
                    get("/actors/#{testy}/acl/#{action.downcase}/actors/#{alice}",
                        :alice).should have_status_code(200).with_body({})
                  end
                else
                  it "returns 404 when not in ACE" do
                    :alice.should directly_have_permission(ace.downcase.to_sym).
                      on_actor(:testy)
                    # Alice does not have other access on testy
                    get("/actors/#{testy}/acl/#{action.downcase}/actors/#{alice}",
                        :alice).should have_status_code(404)
                  end
                end
              end

              context "an actor inderectly in the #{ace} ACE" do
                with_actors :alice, :testy, :bob
                with_group :hackers

                with_ace_on_actor :testy, ace.downcase.to_sym, :groups => [:hackers]
                with_members :hackers, :actors => [:alice]

                if (action == ace)
                  it "returns 200 when in ACE" do
                    :alice.should_not directly_have_permission(ace.downcase.to_sym).
                      on_actor(:testy)
                    :alice.should be_a_direct_member_of(:hackers)
                    :hackers.should directly_have_permission(ace.downcase.to_sym).
                      on_actor(:testy)

                    get("/actors/#{testy}/acl/#{action.downcase}/actors/#{alice}",
                        :alice).should have_status_code(200).with_body({})
                  end
                else
                  it "returns 404 when not in ACE" do
                    :alice.should_not directly_have_permission(ace.downcase.to_sym).
                      on_actor(:testy)
                    :alice.should be_a_direct_member_of(:hackers)
                    :hackers.should directly_have_permission(ace.downcase.to_sym).
                      on_actor(:testy)

                    get("/actors/#{testy}/acl/#{action.downcase}/actors/#{alice}",
                        :alice).should have_status_code(404)
                  end
                end
              end

              context "with a non-existent target" do
                with_actor :alice

                it "can't be read, because it doesn't exist" do
                  fake_actor = "deadbeefdeadbeefdeadbeefdeadbeef"

                  get("/actors/#{fake_actor}/acl/#{action.downcase}/actors/#{alice}",
                      :alice).should have_status_code(404)
                end
              end
            end

            context "for GROUPS member type" do
              context "a group directly in the #{ace} ACE" do
                with_actors :testy, :alice
                with_group :ponies

                with_ace_on_actor :testy, ace.downcase.to_sym, :groups => [:ponies]
                with_members :ponies, :actors => [:alice]

                if (action == ace)
                  it "returns 200 when in ACE" do
                    # My understanding is that if group X has permissions on actor X,
                    # this should return 200, but as it is, it doesn't
                    pending "doesn't seem to work" do
                      :ponies.should directly_have_permission(ace.downcase.to_sym).
                        on_actor(:testy)
                      # Ponies has specific ACE access on testy
                      get("/actors/#{testy}/acl/#{action.downcase}/groups/#{ponies}",
                          :alice).should have_status_code(200).with_body({})
                    end
                  end
                else
                  it "returns 404 when not in ACE" do
                    :ponies.should directly_have_permission(ace.downcase.to_sym).
                      on_actor(:testy)
                    # Ponies does not have other access on testy
                    get("/actors/#{testy}/acl/#{action.downcase}/groups/#{ponies}",
                        :alice).should have_status_code(404)
                  end
                end
              end

              context "a group inderectly in the #{ace} ACE" do
                with_actors :alice, :testy, :bob
                with_groups :hackers, :ponies

                with_ace_on_actor :testy, ace.downcase.to_sym, :groups => [:ponies]
                with_members :ponies, :groups => [:hackers]
                with_members :hackers, :actors => [:alice]

                if (action == ace)
                  # See above
                  it "returns 200 when in ACE" do
                    pending "doesn't seem to work" do
                      :alice.should_not directly_have_permission(ace.downcase.to_sym).
                        on_actor(:testy)
                      :alice.should be_a_direct_member_of(:hackers)
                      :hackers.should be_a_direct_member_of(:ponies)
                      :hackers.should_not directly_have_permission(ace.downcase.to_sym).
                        on_actor(:testy)
                      :ponies.should directly_have_permission(ace.downcase.to_sym).
                        on_actor(:testy)

                      get("/actors/#{testy}/acl/#{action.downcase}/groups/#{hackers}",
                          :alice).should have_status_code(200).with_body({})
                    end
                  end
                else
                  it "returns 404 when not in ACE" do
                    :alice.should_not directly_have_permission(ace.downcase.to_sym).
                      on_actor(:testy)
                    :alice.should be_a_direct_member_of(:hackers)
                    :hackers.should be_a_direct_member_of(:ponies)
                    :hackers.should_not directly_have_permission(ace.downcase.to_sym).
                      on_actor(:testy)
                    :ponies.should directly_have_permission(ace.downcase.to_sym).
                      on_actor(:testy)

                    get("/actors/#{testy}/acl/#{action.downcase}/groups/#{hackers}",
                        :alice).should have_status_code(404)
                  end
                end
              end

              context "with a non-existent target" do
                with_actor :alice
                with_group :ponies

                it "can't be read, because it doesn't exist" do
                  fake_actor = "deadbeefdeadbeefdeadbeefdeadbeef"

                  get("/actors/#{fake_actor}/acl/#{action.downcase}/groups/#{ponies}",
                      :alice).should have_status_code(404)
                end
              end
            end

            # Some tests for an unexpected member_type -- don't really need to test
            # more than one, do we?

            # TODO: think about doing containers, too?

            context "for OBJECT member type" do
              context "an actor directly in the #{ace} ACE" do
                with_actor :testy
                with_object :widget

                it "returns 404 all the time" do
                  get("/actors/#{testy}/acl/#{action.downcase}/objects/#{widget}",
                      :superuser).should have_status_code(404)
                end
              end

              context "with a non-existent target" do
                with_object :widget

                it "can't be read, because it doesn't exist" do
                  fake_actor = "deadbeefdeadbeefdeadbeefdeadbeef"

                  get("/actors/#{fake_actor}/acl/#{action.downcase}/objects/#{widget}",
                      :superuser).should have_status_code(404)
                end
              end
            end
          end
        end
      end
    end

    should_not_allow :POST, "/actors/ffffffffffffffffffffffffffffffff/acl/create/actors/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"
    should_not_allow :PUT, "/actors/ffffffffffffffffffffffffffffffff/acl/create/actors/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"
    should_not_allow :DELETE, "/actors/ffffffffffffffffffffffffffffffff/acl/create/actors/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"
  end # /actors/<actor_id>/acl/<action>/<member_type>/<member_id>
end
