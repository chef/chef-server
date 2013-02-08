require 'pedant/rspec/actors_util.rb'

describe "Actors Endpoint" do
  include Pedant::RSpec::ActorsUtil

  let(:mattdamon) { "deadbeefdeadbeefdeadbeefdeadbeef" }

  context "pedant API sanity check" do
    ['CREATE', 'READ', 'UPDATE', 'DELETE', 'GRANT'].each do |action|
      context "for #{action} ACE" do
        context "for single ACE on actor" do
          with_actors :hasselhoff, :shatner

          with_ace_on_actor :shatner, action.downcase.to_sym, :actors => [:hasselhoff]

          it "has permission" do
            :hasselhoff.should directly_have_permission(action.downcase.to_sym).
              on_actor(:shatner)
          end
        end

        context "an actor NOT in the ACE" do
          with_actors :malkovich, :shatner

          # Give malkovich everything EXCEPT action
          acl = {
            :create => {:actors => [:malkovich], :groups => []},
            :read   => {:actors => [:malkovich], :groups => []},
            :update => {:actors => [:malkovich], :groups => []},
            :delete => {:actors => [:malkovich], :groups => []},
            :grant  => {:actors => [:malkovich], :groups => []}
          }
          acl[action.downcase.to_sym] = {:actors => [], :groups => []}
          
          with_acl_on_actor :shatner, acl

          it "does not have permission" do
            :malkovich.should_not directly_have_permission(action.downcase.to_sym).
              on_actor(:shatner)
          end
        end

        context "an actor indirectly in the ACE" do
          with_actors :hasselhoff, :shatner, :malkovich
          with_group :hipsters

          with_ace_on_actor :shatner, action.downcase.to_sym, :groups => [:hipsters]
          with_members :hipsters, :actors => [:hasselhoff]

          it "has only indirect permission" do
            :hasselhoff.should_not directly_have_permission(action.downcase.to_sym).
              on_actor(:shatner)
            :hasselhoff.should be_a_direct_member_of(:hipsters)
            :hipsters.should directly_have_permission(action.downcase.to_sym).
              on_actor(:shatner)
          end
        end

        context "an actor doubly-indirectly in the ACE" do
          with_actors :hasselhoff, :shatner
          with_groups :hipsters, :brogrammers

          with_ace_on_actor :shatner, action.downcase.to_sym, :groups => [:brogrammers]
          with_members :brogrammers, :groups => [:hipsters]
          with_members :hipsters, :actors => [:hasselhoff]

          it "has only doubly-indirect permission" do
            :hasselhoff.should_not directly_have_permission(action.downcase.to_sym).
              on_actor(:shatner)
            :hasselhoff.should be_a_direct_member_of(:hipsters)
            :hipsters.should be_a_direct_member_of(:brogrammers)
            :hipsters.should_not directly_have_permission(action.downcase.to_sym).
              on_actor(:shatner)
            :brogrammers.should directly_have_permission(action.downcase.to_sym).
              on_actor(:shatner)
          end
        end
      end
    end

    context "an actor with NO ACE" do
      with_actors :malkovich, :shatner

      # Give malkovich no access at all
      with_acl_on_actor :shatner, {
        :create => {:actors => [], :groups => []},
        :read   => {:actors => [], :groups => []},
        :update => {:actors => [], :groups => []},
        :delete => {:actors => [], :groups => []},
        :grant  => {:actors => [], :groups => []}
      }

      it "has no permissions" do
        :malkovich.should_not directly_have_permission(:create).on_actor(:shatner) 
        :malkovich.should_not directly_have_permission(:read).on_actor(:shatner)
        :malkovich.should_not directly_have_permission(:update).on_actor(:shatner)
        :malkovich.should_not directly_have_permission(:delete).on_actor(:shatner)
        :malkovich.should_not directly_have_permission(:grant).on_actor(:shatner)
      end
    end

    context "an actor with full ACE" do
      with_actors :hasselhoff, :shatner

      # Give hasselhoff full access
      with_acl_on_actor :shatner, {
        :create => {:actors => [:hasselhoff], :groups => []},
        :read   => {:actors => [:hasselhoff], :groups => []},
        :update => {:actors => [:hasselhoff], :groups => []},
        :delete => {:actors => [:hasselhoff], :groups => []},
        :grant  => {:actors => [:hasselhoff], :groups => []}
      }

      it "has all permissions" do
        :hasselhoff.should directly_have_permission(:create).on_actor(:shatner) 
        :hasselhoff.should directly_have_permission(:read).on_actor(:shatner)
        :hasselhoff.should directly_have_permission(:update).on_actor(:shatner)
        :hasselhoff.should directly_have_permission(:delete).on_actor(:shatner)
        :hasselhoff.should directly_have_permission(:grant).on_actor(:shatner)
      end
    end
  end

  context "/actors" do
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
        fake_actor = :mattdamon

        creates_an_actor_as(fake_actor)
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
        with_actor :shatner

        it "is in own ACEs" do
          :shatner.should directly_have_permission(:create).on_actor(:shatner)
          :shatner.should directly_have_permission(:read).on_actor(:shatner)
          :shatner.should directly_have_permission(:update).on_actor(:shatner)
          :shatner.should directly_have_permission(:delete).on_actor(:shatner)
          :shatner.should directly_have_permission(:grant).on_actor(:shatner)
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
    # GET returns the actor
    #
    # NOTE: This is borderline pointless, since it always returns an
    # empty JSON object!  It can basically only answer the question
    # "Is there an actor with this ID?"
    context "GET" do
      context "an actor directly in the READ ACE" do
        with_actors :hasselhoff, :shatner

        with_ace_on_actor :shatner, :read, :actors => [:hasselhoff]

        it "can read the actor" do
          get("/actors/#{shatner}",
              :hasselhoff).should have_status_code(200).with_body({})
        end
      end

      context "an actor NOT in the READ ACE" do
        with_actors :malkovich, :shatner

        # Give malkovich everything EXCEPT read
        with_acl_on_actor :shatner, {
          :create => {:actors => [:malkovich], :groups => []},
          :read   => {:actors => [],     :groups => []}, # <--- That's the one!
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
        with_group :hipsters

        with_ace_on_actor :shatner, :read, :groups => [:hipsters]
        with_members :hipsters, :actors => [:hasselhoff]

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
    # TODO: How does this affect ACLs the actor is a member of?
    context "DELETE" do

      context "an actor directly in the DELETE ACE" do
        with_actors :hasselhoff, :shatner

        with_ace_on_actor :shatner, :delete, :actors => [:hasselhoff]

        it "can delete the actor" do
          delete("/actors/#{shatner}",
                 :hasselhoff).should have_status_code(200).with_body({})
          get("/actors/#{shatner}", :superuser).should have_status_code(404)
        end
      end

      context "an actor NOT in the DELETE ACE" do
        with_actors :malkovich, :shatner

        # Give malkovich everything EXCEPT delete
        with_acl_on_actor :shatner, {
          :create => {:actors => [:malkovich], :groups => []},
          :read   => {:actors => [:malkovich], :groups => []},
          :update => {:actors => [:malkovich], :groups => []},
          :delete => {:actors => [],     :groups => []}, # <--- That's the one!
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
        with_group :hipsters

        with_ace_on_actor :shatner, :delete, :groups => [:hipsters]
        with_members :hipsters, :actors => [:hasselhoff]

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
      ['CREATE', 'READ', 'UPDATE', 'DELETE', 'GRANT'].each do |ace|

        context "an actor directly in the #{ace} ACE" do
          with_actors :hasselhoff, :shatner

          with_ace_on_actor :shatner, ace.downcase.to_sym, :actors => [:hasselhoff]

          it "can read the acl" do
            body = {
              "create" => {"actors" => [shatner], "groups" => []},
              "read" => {"actors" => [shatner], "groups" => []},
              "update" => {"actors" => [shatner], "groups" => []},
              "delete" => {"actors" => [shatner], "groups" => []},
              "grant" => {"actors" => [shatner], "groups" => []}
            }
            body[ace.downcase] = {"actors" => [hasselhoff], "groups" => []}

            get("/actors/#{shatner}/acl",
                :hasselhoff).should have_status_code(200).with_body(body)
          end
        end

        context "an actor indirectly in the #{ace} ACE" do
          with_actors :hasselhoff, :shatner
          with_group :hipsters

          with_ace_on_actor :shatner, ace.downcase.to_sym, :groups => [:hipsters]
          with_members :hipsters, :actors => [:hasselhoff]

          it "can read the acl" do
            body = {
              "create" => {"actors" => [shatner], "groups" => []},
              "read" => {"actors" => [shatner], "groups" => []},
              "update" => {"actors" => [shatner], "groups" => []},
              "delete" => {"actors" => [shatner], "groups" => []},
              "grant" => {"actors" => [shatner], "groups" => []}
            }
            body[ace.downcase] = {"actors" => [], "groups" => [hipsters]}

            get("/actors/#{shatner}/acl",
                :hasselhoff).should have_status_code(200).with_body(body)
          end
        end
      end

      context "an actor with NO ACE" do
        with_actors :malkovich, :shatner

        # Give malkovich no access at all
        with_acl_on_actor :shatner, {
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
              with_actors :hasselhoff, :shatner

              with_ace_on_actor :shatner, ace.downcase.to_sym, :actors => [:hasselhoff]

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

            context "an actor indirectly in the #{ace} ACE" do
              with_actors :hasselhoff, :shatner
              with_group :hipsters

              with_ace_on_actor :shatner, ace.downcase.to_sym, :groups => [:hipsters]
              with_members :hipsters, :actors => [:hasselhoff]

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
              with_acl_on_actor :shatner, {
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

    should_not_allow :POST, "/actors/ffffffffffffffffffffffffffffffff/acl/create"

    # PUT replaces an ACE atomically

    context "PUT" do
      ['CREATE', 'READ', 'UPDATE', 'DELETE', 'GRANT'].each do |action|
        context "for #{action} action" do

          # TODO: probably want to expand this with various types of bad input,
          # although at the moment pretty much anything at all will crash it
          context "an actor directly in the GRANT ACE, with bad input" do
            with_actors :hasselhoff, :shatner

            with_ace_on_actor :shatner, :grant, :actors => [:hasselhoff]

            it "returns 400" do
              pending "returns 500 instead" do
                put("/actors/#{shatner}/acl/#{action.downcase}",
                    :hasselhoff, :payload => {}).
                  should have_status_code(400).with_body({"error" => "bad input"})

                get("/actors/#{shatner}/acl/#{action.downcase}",
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
            with_actors :hasselhoff, :shatner

            with_ace_on_actor :shatner, :grant, :actors => [:hasselhoff]

            it "returns 400" do
              pending "returns 200 instead" do
                put("/actors/#{shatner}/acl/#{action.downcase}",
                    :hasselhoff,
                    :payload => {"actors" => ["zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz"],
                      "groups" => []}).
                  should have_status_code(400).with_body({"error" => "bad input"})

                get("/actors/#{shatner}/acl/#{action.downcase}",
                    :superuser).should have_status_code(200).
                  with_body({"actors" => [hasselhoff], "groups" => []})
              end
            end
          end

          context "an actor directly in the GRANT ACE, with invalid group" do
            with_actors :hasselhoff, :shatner

            with_ace_on_actor :shatner, :grant, :actors => [:hasselhoff]

            it "returns 400" do
              pending "returns 200 instead" do
                put("/actors/#{shatner}/acl/#{action.downcase}",
                    :hasselhoff, :payload => {"actors" => [],
                      "groups" => ["zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz"]}).
                  should have_status_code(400).with_body({"error" => "bad input"})

                get("/actors/#{shatner}/acl/#{action.downcase}",
                    :superuser).should have_status_code(200).
                  with_body({"actors" => [hasselhoff], "groups" => []})
              end
            end
          end

          context "an actor directly in the GRANT ACE, with non-existent actor" do
            with_actors :hasselhoff, :shatner

            with_ace_on_actor :shatner, :grant, :actors => [:hasselhoff]

            it "returns 400" do
              pending "returns 200 instead" do
                put("/actors/#{shatner}/acl/#{action.downcase}",
                    :hasselhoff,
                    :payload => {"actors" => ["ffffffffffffffffffffffffffffffff"],
                      "groups" => []}).
                  should have_status_code(400).with_body({"error" => "bad input"})

                get("/actors/#{shatner}/acl/#{action.downcase}",
                    :superuser).should have_status_code(200).
                  with_body({"actors" => [hasselhoff], "groups" => []})
              end
            end
          end

          context "an actor directly in the GRANT ACE, with non-existent group" do
            with_actors :hasselhoff, :shatner

            with_ace_on_actor :shatner, :grant, :actors => [:hasselhoff]

            it "returns 400" do
              pending "returns 200 instead" do
                put("/actors/#{shatner}/acl/#{action.downcase}",
                    :hasselhoff, :payload => {"actors" => [],
                      "groups" => ["ffffffffffffffffffffffffffffffff"]}).
                  should have_status_code(400).with_body({"error" => "bad input"})

                get("/actors/#{shatner}/acl/#{action.downcase}",
                    :superuser).should have_status_code(200).
                  with_body({"actors" => [hasselhoff], "groups" => []})
              end
            end
          end

          context "an actor directly in the GRANT ACE, modifying actors" do
            with_actors :hasselhoff, :schwartzenegger, :shatner

            with_ace_on_actor :shatner, :grant, :actors => [:hasselhoff]

            it "can modify the ACE for actors" do
              put("/actors/#{shatner}/acl/#{action.downcase}",
                  :hasselhoff, :payload => {"actors" => [schwartzenegger],
                    "groups" => []}).
                should have_status_code(200).with_body({})

              get("/actors/#{shatner}/acl/#{action.downcase}",
                  :superuser).should have_status_code(200).
                with_body({"actors" => [schwartzenegger], "groups" => []})
            end
          end

          context "an actor directly in the GRANT ACE, modifying groups" do
            with_actors :hasselhoff, :shatner
            with_group :brogrammers

            with_ace_on_actor :shatner, :grant, :actors => [:hasselhoff]

            it "can modify the ACE for groups" do
              put("/actors/#{shatner}/acl/#{action.downcase}",
                  :hasselhoff, :payload => {"actors" => [], "groups" => [brogrammers]}).
                should have_status_code(200).with_body({})

              get("/actors/#{shatner}/acl/#{action.downcase}",
                  :superuser).should have_status_code(200).
                with_body({"actors" => [], "groups" => [brogrammers]})
            end
          end

          context "an actor NOT in the GRANT ACE" do
            with_actors :malkovich, :schwartzenegger, :shatner

            # Give malkovich everything EXCEPT grant
            with_acl_on_actor :shatner, {
              :create => {:actors => [:malkovich], :groups => []},
              :read   => {:actors => [:malkovich], :groups => []},
              :update => {:actors => [:malkovich], :groups => []},
              :delete => {:actors => [:malkovich], :groups => []},
              :grant  => {:actors => [],     :groups => []} # <--- That's the one!
            }

            if (action == 'GRANT')
              let(:body) { {"actors" => [], "groups" => []} }
            else
              let(:body) { {"actors" => [malkovich], "groups" => []} }
            end

            it "cannot modify the ACE" do
              put("/actors/#{shatner}/acl/#{action.downcase}",
                  :malkovich, :payload => {"actors" => [schwartzenegger],
                    "groups" => []}).should have_status_code(403).
                with_body({"error" => "must be in the grant access control entry to perform this action"})

              get("/actors/#{shatner}/acl/#{action.downcase}",
                  :superuser).should have_status_code(200).with_body(body)
            end
          end

          context "an actor indirectly in the GRANT ACE, modifying actors" do
            with_actors :hasselhoff, :shatner, :malkovich, :norris
            with_groups :hipsters

            with_ace_on_actor :shatner, :grant, :groups => [:hipsters]
            with_members :hipsters, :actors => [:hasselhoff]

            it "can modify the ACE for actors" do
              put("/actors/#{shatner}/acl/#{action.downcase}",
                     :hasselhoff, :payload => {"actors" => [norris], "groups" => []}).
                should have_status_code(200)
              get("/actors/#{shatner}/acl/#{action.downcase}",
                  :superuser).should have_status_code(200).
                with_body({"actors" => [norris], "groups" => []})
            end
          end

          context "an actor indirectly in the GRANT ACE, modifying groups" do
            with_actors :hasselhoff, :shatner, :malkovich
            with_groups :hipsters, :brogrammers

            with_ace_on_actor :shatner, :grant, :groups => [:hipsters]
            with_members :hipsters, :actors => [:hasselhoff]

            it "can modify the ACE for groups" do
              put("/actors/#{shatner}/acl/#{action.downcase}",
                     :hasselhoff, :payload => {"actors" => [],
                    "groups" => [brogrammers]}).should have_status_code(200)

              get("/actors/#{shatner}/acl/#{action.downcase}",
                  :superuser).should have_status_code(200).
                with_body({"actors" => [], "groups" => [brogrammers]})
            end
          end

          context "with a non-existent target" do
            with_actor :hasselhoff

            it "can't modify its ACE, because it doesn't exist" do
              fake_actor = mattdamon

              # Prove it doesn't exist
              get("/actors/#{fake_actor}/acl/#{action.downcase}",
                  :hasselhoff).should have_status_code(404)

              # Now try to delete it
              delete("/actors/#{fake_actor}/acl/#{action.downcase}",
                     :hasselhoff).should have_status_code(404)
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
            with_actors :hasselhoff, :shatner

            with_ace_on_actor :shatner, :grant, :actors => [:hasselhoff]

            it "can clear the ACE" do
              pending "causes internal 500 errors" do
                delete("/actors/#{shatner}/acl/#{action.downcase}",
                       :hasselhoff).should have_status_code(200).with_body({})
                get("/actors/#{shatner}/acl/#{action.downcase}",
                    :superuser).should have_status_code(404)
              end
            end
          end

          context "an actor NOT in the GRANT ACE" do
            with_actors :malkovich, :shatner

            # Give malkovich everything EXCEPT grant
            with_acl_on_actor :shatner, {
              :create => {:actors => [:malkovich], :groups => []},
              :read   => {:actors => [:malkovich], :groups => []},
              :update => {:actors => [:malkovich], :groups => []},
              :delete => {:actors => [:malkovich], :groups => []},
              :grant  => {:actors => [],     :groups => []} # <--- That's the one!
            }

            it "cannot clear the ACE" do
              delete("/actors/#{shatner}/acl/#{action.downcase}",
                     :malkovich).should have_status_code(403).
                with_body({"error" => "must be in the grant access control entry to perform this action"})

              get("/actors/#{shatner}/acl/#{action.downcase}",
                  :superuser).should have_status_code(200)
            end
          end

          context "an actor indirectly in the GRANT ACE" do
            with_actors :hasselhoff, :shatner
            with_group :hipsters

            with_ace_on_actor :shatner, :grant, :groups => [:hipsters]
            with_members :hipsters, :actors => [:hasselhoff]

            it "can clear the ACE" do
              pending "causes internal 500 errors" do
                delete("/actors/#{shatner}/acl/#{action.downcase}",
                       :hasselhoff).should have_status_code(200).with_body({})

                get("/actors/#{shatner}/acl/#{action.downcase}",
                    :superuser).should have_status_code(404)
              end
            end
          end

          context "with a non-existent target" do
            with_actor :hasselhoff

            it "can't clear its ACE, because it doesn't exist" do
              fake_actor = mattdamon

              # Prove it doesn't exist
              get("/actors/#{fake_actor}/acl/#{action.downcase}",
                  :hasselhoff).should have_status_code(404)

              # Now try to delete it
              delete("/actors/#{fake_actor}/acl/#{action.downcase}",
                     :hasselhoff).should have_status_code(404)
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
            with_actor :shatner

            it "get should not be found" do
              get("/actors/#{shatner}/acl/#{action.downcase}/#{type.downcase}/",
                  :superuser).should have_status_code(404)
            end

            it "post should not be found" do
              post("/actors/#{shatner}/acl/#{action.downcase}/#{type.downcase}/",
                   :superuser).should have_status_code(404)
            end

            it "put should not be found" do
              put("/actors/#{shatner}/acl/#{action.downcase}/#{type.downcase}/",
                  :superuser).should have_status_code(404)
            end

            it "delete should not be found" do
              delete("/actors/#{shatner}/acl/#{action.downcase}/#{type.downcase}/",
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
                with_actors :hasselhoff, :shatner

                with_ace_on_actor :shatner, ace.downcase.to_sym, :actors => [:hasselhoff]

                if (action == ace)
                  it "returns 200 when in ACE" do
                    # Hasselhoff has specific ACE access on shatner
                    get("/actors/#{shatner}/acl/#{action.downcase}/actors/#{hasselhoff}",
                        :hasselhoff).should have_status_code(200).with_body({})
                  end
                else
                  it "returns 404 when not in ACE" do
                    # Hasselhoff does not have other access on shatner
                    get("/actors/#{shatner}/acl/#{action.downcase}/actors/#{hasselhoff}",
                        :hasselhoff).should have_status_code(404)
                  end
                end
              end

              context "an actor indirectly in the #{ace} ACE" do
                with_actors :hasselhoff, :shatner
                with_group :hipsters

                with_ace_on_actor :shatner, ace.downcase.to_sym, :groups => [:hipsters]
                with_members :hipsters, :actors => [:hasselhoff]

                if (action == ace)
                  it "returns 200 when in ACE" do
                    get("/actors/#{shatner}/acl/#{action.downcase}/actors/#{hasselhoff}",
                        :hasselhoff).should have_status_code(200).with_body({})
                  end
                else
                  it "returns 404 when not in ACE" do
                    get("/actors/#{shatner}/acl/#{action.downcase}/actors/#{hasselhoff}",
                        :hasselhoff).should have_status_code(404)
                  end
                end
              end

              context "with a non-existent target" do
                with_actor :hasselhoff

                it "can't be read, because it doesn't exist" do
                  fake_actor = mattdamon

                  get("/actors/#{fake_actor}/acl/#{action.downcase}/actors/#{hasselhoff}",
                      :hasselhoff).should have_status_code(404)
                end
              end
            end

            context "for GROUPS member type" do
              context "a group directly in the #{ace} ACE" do
                with_actors :shatner, :hasselhoff
                with_group :brogrammers

                with_ace_on_actor :shatner, ace.downcase.to_sym, :groups => [:brogrammers]
                with_members :brogrammers, :actors => [:hasselhoff]

                if (action == ace)
                  it "returns 200 when in ACE" do
                    # My understanding is that if group X has permissions on actor X,
                    # this should return 200, but as it is, it doesn't
                    pending "doesn't seem to work" do
                      # Brogrammers has specific ACE access on shatner
                      get("/actors/#{shatner}/acl/#{action.downcase}/groups/#{brogrammers}",
                          :hasselhoff).should have_status_code(200).with_body({})
                    end
                  end
                else
                  it "returns 404 when not in ACE" do
                    # Brogrammers does not have other access on shatner
                    get("/actors/#{shatner}/acl/#{action.downcase}/groups/#{brogrammers}",
                        :hasselhoff).should have_status_code(404)
                  end
                end
              end

              context "a group indirectly in the #{ace} ACE" do
                with_actors :hasselhoff, :shatner
                with_groups :hipsters, :brogrammers

                with_ace_on_actor :shatner, ace.downcase.to_sym, :groups => [:brogrammers]
                with_members :brogrammers, :groups => [:hipsters]
                with_members :hipsters, :actors => [:hasselhoff]

                if (action == ace)
                  # See above
                  it "returns 200 when in ACE" do
                    pending "doesn't seem to work" do
                      get("/actors/#{shatner}/acl/#{action.downcase}/groups/#{hipsters}",
                          :hasselhoff).should have_status_code(200).with_body({})
                    end
                  end
                else
                  it "returns 404 when not in ACE" do
                    get("/actors/#{shatner}/acl/#{action.downcase}/groups/#{hipsters}",
                        :hasselhoff).should have_status_code(404)
                  end
                end
              end

              context "with a non-existent target" do
                with_actor :hasselhoff
                with_group :brogrammers

                it "can't be read, because it doesn't exist" do
                  fake_actor = mattdamon

                  get("/actors/#{fake_actor}/acl/#{action.downcase}/groups/#{brogrammers}",
                      :hasselhoff).should have_status_code(404)
                end
              end
            end

            # Some tests for an unexpected member_type -- don't really need to test
            # more than one, do we?

            # TODO: think about doing containers, too?

            context "for OBJECT member type" do
              context "an actor directly in the #{ace} ACE" do
                with_actor :shatner
                with_object :spork

                it "returns 404 all the time" do
                  get("/actors/#{shatner}/acl/#{action.downcase}/objects/#{spork}",
                      :superuser).should have_status_code(404)
                end
              end

              context "with a non-existent target" do
                with_object :spork

                it "can't be read, because it doesn't exist" do
                  fake_actor = mattdamon

                  get("/actors/#{fake_actor}/acl/#{action.downcase}/objects/#{spork}",
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
