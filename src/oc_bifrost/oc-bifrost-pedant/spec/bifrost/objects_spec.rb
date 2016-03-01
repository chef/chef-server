describe "Objects Endpoint" do
  let(:mattdamon) { "deadbeefdeadbeefdeadbeefdeadbeef" }
  let(:toupee) { "beefdeadbeefdeadbeefdeadbeefdead" }

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
                           with_body({"id" => /^[0-9a-f]{32}$/, "uri" => /[0-9a-fhttp:\\]*/})
          # TODO URI: URI code broken
          #"uri" => /^#{Pedant.config[:host]}:#{Pedant.config[:port]}\/objects\/[0-9a-f]{32}$/})
        
          @object_id = parse(response)["id"]

          # Verify that uri and id are the same
          uri_id = parse(response)["uri"].split("/")[-1]
          uri_id.should == @object_id
        end
      end

      context "as a superuser" do
        creates_object_as(:superuser)
      end

      # This is one of the actual changes in behavior between old authz and new V1
      # of Bifrost; this actually works with the old server, but it can't with the
      # new schema because it's not possible to put bogus ACLs in the database which
      # this would require
      context "as an unknown requestor" do
        let(:fake_actor) { mattdamon }

        it "should not create an object" do
          response = post("/objects", fake_actor)

          response.should have_status_code(401).
            with_body({"error" => "requesting actor id of '#{fake_actor}' does not exist"})
        end
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

        with_ace_on :spork, :read, :to => :hasselhoff

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
        with_group :hipsters, :members => [:hasselhoff]
        with_object :spork

        with_ace_on :spork, :read, :to => :hipsters

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

        with_ace_on :spork, :delete, :to => :hasselhoff

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
        with_group :hipsters, :members => [:hasselhoff]
        with_object :spork

        with_ace_on :spork, :delete, :to => :hipsters

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
    end # DELETE
  end # /objects/<object_id>
end
