describe "Containers Endpoint" do
  let(:mattdamon) { "deadbeefdeadbeefdeadbeefdeadbeef" }
  let(:colander) { "beefdeadbeefdeadbeefdeadbeefdead" }

  context "/containers" do
    # What we are testing:

    # Here we test container creation (all other HTTP verbs should be
    # disallowed), making sure the response body is correct and that
    # id and id in the uri match, as well as basic header validation,
    # as well as making sure that the requesting actor is contained in
    # the newly created container's ACLs.

    should_not_allow :GET, "/containers"

    context "POST" do

      # We mainly do this to make sure the test cleans up after
      # itself; otherwise we have to repeat the hacky after :each with
      # the @container_id stuff, and, well this is pretty much the same
      # for every creation
      def self.creates_container_as(requestor, headers = {})
        after :each do
          delete("/containers/#{@container_id}", :superuser)
        end

        it "creates a container" do
          response = post("/containers", requestor, headers)

          # TODO: de-hardcode uri hostname in response body, make configurable
          response.should have_status_code(201).
                           with_body({"id" => /^[0-9a-f]{32}$/, "uri" => /[0-9a-fhttp:\\]*/})
          # TODO URI: URI code broken
          #"uri" => /^#{Pedant.config[:host]}:#{Pedant.config[:port]}\/containers\/[0-9a-f]{32}$/})
        
          @container_id = parse(response)["id"]

          # Verify that uri and id are the same
          uri_id = parse(response)["uri"].split("/")[-1]
          uri_id.should == @container_id
        end
      end

      context "as a superuser" do
        creates_container_as(:superuser)
      end

      # This is one of the actual changes in behavior between old authz and new V1
      # of Bifrost; this actually works with the old server, but it can't with the
      # new schema because it's not possible to put bogus ACLs in the database which
      # this would require
      context "as an unknown requestor" do
        let(:fake_actor) { mattdamon }

        it "should not create a container" do
          response = post("/containers", fake_actor)

          response.should have_status_code(401).
            with_body({"error" => "requesting actor id of '#{fake_actor}' does not exist"})
        end
      end

      context "without the X-Ops-Requesting-Actor-Id header" do
        it "should not create a container" do
          response = post("/containers", :superuser,
                          :merge_headers => {"X-Ops-Requesting-Actor-Id" => :DELETE})

          response.should have_status_code(403).
            with_body({"error" => "must specify a requesting actor id"})
        end
      end

      # Not quite clear the purpose of this header, actually
      context "without the X-Ops-User-Id header" do
        creates_container_as(:superuser,
                             :merge_headers => {"X-Ops-User-Id" => :DELETE})
      end

      context "without ANY of the standard headers except Content-Type" do
        it "should not create a container" do
          response = post("/containers", :superuser,
                          :headers => {"Content-Type" => "application/json"})

          response.should have_status_code(403).
            with_body({"error" => "must specify a requesting actor id"})
        end
      end

      context "without any headers" do
        it "should not create a container" do
          post("/containers", :superuser, :headers => {}).should have_status_code(403).
            with_body({"error" => "must specify a requesting actor id"})
        end
      end

      context "created container" do
        with_actor :shatner

        before :each do
          response = post("/containers", shatner)
          @container = parse(response)["id"]
        end

        after :each do
          delete("/containers/#{@container}", shatner)
        end

        it "contains creator in ACLs" do
          body = {"create" => {"actors" => [shatner], "groups" => []},
            "read" => {"actors" => [shatner], "groups" => []},
            "update" => {"actors" => [shatner], "groups" => []},
            "delete" => {"actors" => [shatner], "groups" => []},
            "grant" => {"actors" => [shatner], "groups" => []}}
          
          get("/containers/#{@container}/acl",
              :superuser).should have_status_code(200).with_body(body)
        end
      end
    end # POST

    should_not_allow :PUT, "/containers"
    should_not_allow :DELETE, "/containers"
  end # /containers

  context "/containers/<container_id>" do
    # What we are testing:

    # Here we test container existence with GET (should require
    # appropriate READ access), as well as the ability to delete
    # containers (should require appropriate DELETE access).  All other
    # HTTP verbs should be disallowed.
    context "GET" do
      context "an actor directly in the READ ACE" do
        with_actor :hasselhoff
        with_container :bucket

        with_ace_on :bucket, :read, :to => :hasselhoff

        it "can read the container" do
          get("/containers/#{bucket}",
              :hasselhoff).should have_status_code(200).with_body({})
        end
      end

      context "an actor NOT in the READ ACE" do
        with_actor :malkovich
        with_container :bucket

        # Give malkovich everything EXCEPT read
        with_acl_on :bucket, {
          :create => {:actors => [:malkovich], :groups => []},
          :read   => {:actors => [],           :groups => []}, # <--- That's the one!
          :update => {:actors => [:malkovich], :groups => []},
          :delete => {:actors => [:malkovich], :groups => []},
          :grant  => {:actors => [:malkovich], :groups => []}
        }

        it "cannot read the container" do
          get("/containers/#{bucket}", :malkovich).should have_status_code(403).
            with_body({"error" => "must be in the read access control entry to perform this action"})
        end
      end

      context "an actor indirectly in the READ ACE" do
        with_actor :hasselhoff
        with_group :hipsters, :members => [:hasselhoff]
        with_container :bucket

        with_ace_on :bucket, :read, :to => :hipsters

        it "can read the container" do
          get("/containers/#{bucket}",
              :hasselhoff).should have_status_code(200).with_body({})
        end
      end

      context "with a non-existent target" do
        with_actor :hasselhoff

        it "can't be read, because it doesn't exist" do
          fake_container = colander

          get("/containers/#{fake_container}", :hasselhoff).should have_status_code(404)
        end
      end
    end # GET

    should_not_allow :POST, "/containers/fake"
    should_not_allow :PUT, "/containers/fake"

    # NOTE: Do we ever actually delete containers?
    #
    # According to comments for oc_chef_authz:delete_resource/3, we do
    # not, so we may very well be able to disallow DELETE as well.
    context "DELETE" do
      context "an actor directly in the DELETE ACE" do
        with_actor :hasselhoff
        with_container :bucket

        with_ace_on :bucket, :delete, :to => :hasselhoff

        it "can delete the container" do
          delete("/containers/#{bucket}",
                 :hasselhoff).should have_status_code(200).with_body({})
          get("/containers/#{bucket}", :superuser).should have_status_code(404)
        end
      end

      context "an actor NOT in the DELETE ACE" do
        with_actor :malkovich
        with_container :bucket

        # Give malkovich everything EXCEPT delete
        with_acl_on :bucket, {
          :create => {:actors => [:malkovich], :groups => []},
          :read   => {:actors => [:malkovich], :groups => []},
          :update => {:actors => [:malkovich], :groups => []},
          :delete => {:actors => [],           :groups => []}, # <--- That's the one!
          :grant  => {:actors => [:malkovich], :groups => []}
        }

        it "cannot delete the container" do
          delete("/containers/#{bucket}", :malkovich).should have_status_code(403).
            with_body({"error" => "must be in the delete access control entry to perform this action"})
          get("/containers/#{bucket}", :superuser).should have_status_code(200)
        end
      end

      context "an actor indirectly in the DELETE ACE" do
        with_actor :hasselhoff
        with_group :hipsters, :members => [:hasselhoff]
        with_container :bucket

        with_ace_on :bucket, :delete, :to => :hipsters

        it "can delete the container" do
          delete("/containers/#{bucket}",
                 :hasselhoff).should have_status_code(200).with_body({})
          get("/containers/#{bucket}", :superuser).should have_status_code(404)
        end
      end

      context "with a non-existent target" do
        with_actor :hasselhoff

        it "can't be deleted, because it doesn't exist" do
          fake_container = colander

          # Prove it doesn't exist
          get("/containers/#{fake_container}", :hasselhoff).should have_status_code(404)

          # Now try to delete it
          delete("/containers/#{fake_container}",
                 :hasselhoff).should have_status_code(404)
        end
      end
    end # DELETE
  end # /containers/<container_id>
end
