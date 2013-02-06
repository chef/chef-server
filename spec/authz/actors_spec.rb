require 'pedant/rspec/actors_util.rb'

describe "Actors Endpoint" do
  include Pedant::RSpec::ActorsUtil

  let(:requestor){superuser}

  context "/actors" do
    let(:request_url){"/actors"}

    should_not_allow :GET, "/actors"

    # POST creates a new actor
    #
    # NOTE: the return value for this a {"id": ID, "uri": URI} hash
    context "POST" do
      let(:request_method){:POST}

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
                 :headers => {}).should have_status_code(400).with_body({"error" => "That ain't right"})
            # Obviously this is not the EXACT response that should come back...
          end
        end
      end
    end

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

        # Give bob everything EXCEPT delete
        with_acl_on_actor :testy, {
          :create => {:actors => [:bob], :groups => []},
          :read   => {:actors => [],     :groups => []}, # <--- That's the one!
          :update => {:actors => [:bob], :groups => []},
          :delete => {:actors => [:bob], :groups => []},
          :grant  => {:actors => [:bob], :groups => []}
        }

        it "cannot read the actor" do
          :bob.should_not directly_have_permission(:read).on_actor(:testy)
          get("/actors/#{testy}", :bob).should have_status_code(403).with_body({"error" => "must be in the read access control entry to perform this action"})
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
    end

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
          delete("/actors/#{testy}", :bob).should have_status_code(403).with_body({"error" => "must be in the delete access control entry to perform this action"})
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
    end
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
    context "GET"

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
    context "GET"

    should_not_allow :POST, "/actors/ffffffffffffffffffffffffffffffff/acl/create"

    # PUT replaces an ACE atomically
    context "PUT"

    # DELETE clears actors and groups from ACE
    context "DELETE"
  end # /actors/<actor_id>/acl/<action>

  context "/actors/<actor_id>/acl/<action>/<member_type>" do
    # TODO: make sure there are no permissions here
  end # /actors/<actor_id>/acl/<action>/<member_type>

  context "/actors/<actor_id>/acl/<action>/<member_type>/<member_id>" do

    # GET uses is_authorized_on_object to determine whether the
    # specified actor / group has the specified permission
    #
    # Returns a 200 and an empty JSON hash if the actor / group has
    # the permission
    #
    # TODO: Perhaps use 204 (OK, No Content) instead?
    context "GET"

    should_not_allow :POST, "/actors/ffffffffffffffffffffffffffffffff/acl/create/actors/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"
    should_not_allow :PUT, "/actors/ffffffffffffffffffffffffffffffff/acl/create/actors/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"
    should_not_allow :DELETE, "/actors/ffffffffffffffffffffffffffffffff/acl/create/actors/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"
  end # /actors/<actor_id>/acl/<action>/<member_type>/<member_id>

end
