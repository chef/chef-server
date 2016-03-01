describe "Bulk Permission Tests" do
  let(:easterbunny) { "deaddeaddeaddeaddeaddeaddeaddead" }
  let(:max_headroom) { "deadbeefdeadbeefdeadbeefdeadbeef" }

  context "/bulk" do
    # What we are testing:

    # Testing the bulk permission endpoint.  We test POST (there are no other
    # working HTTP verbs), testing for correctness and responses to malformed
    # requests, as well as for permissions (i.e., that everyone can access it, since
    # we don't restrict it).

    context "POST" do
      context "sanity tests" do
        context "permissions" do
          with_actors :shatner, :hasselhoff, :nemoy
          with_ace_on :hasselhoff, :read, :to => :shatner

          it "requestor can read itself" do
            request = {
              "requestor_id" => shatner,
              "permission" => 'read',
              "type" => 'actor',
              "collection" => [hasselhoff]
            }
            post("/bulk", :shatner,
              :payload => request).should have_status_code(204)
          end

          it "target can read it" do
            request = {
              "requestor_id" => shatner,
              "permission" => 'read',
              "type" => 'actor',
              "collection" => [hasselhoff]
            }
            post("/bulk", :hasselhoff,
              :payload => request).should have_status_code(204)
          end

          it "third party can read it" do
            request = {
              "requestor_id" => shatner,
              "permission" => 'read',
              "type" => 'actor',
              "collection" => [hasselhoff]
            }
            post("/bulk", :nemoy,
              :payload => request).should have_status_code(204)
          end

          it "superuser can read it" do
            request = {
              "requestor_id" => shatner,
              "permission" => 'read',
              "type" => 'actor',
              "collection" => [hasselhoff]
            }
            post("/bulk", :superuser,
              :payload => request).should have_status_code(204)
          end
        end

        context "malformed requests" do
          with_actors :shatner, :hasselhoff
          with_ace_on :hasselhoff, :read, :to => :shatner

          it "empty body returns 400" do
            request = ''
            post("/bulk", :superuser,
              :payload => request).should have_status_code(400).
              with_body({"error" => "invalid JSON in request body"})
          end

          it "invalid body returns 400" do
            request = 'bogus'
            post("/bulk", :superuser,
              :payload => request).should have_status_code(400).
              with_body({"error" => "invalid JSON in request body"})
          end

          it "empty body object returns 400" do
            request = {}
            post("/bulk", :superuser,
              :payload => request).should have_status_code(400).
              with_body({"error" => "required request key 'requestor_id' is missing"})
          end

          it "missing requestor returns 400" do
            request = {
              "permission" => 'read',
              "type" => 'actor',
              "collection" => [hasselhoff]
            }
            post("/bulk", :superuser,
              :payload => request).should have_status_code(400).
              with_body({"error" => "required request key 'requestor_id' is missing"})
          end

          it "bad requestor returns 400" do
            request = {
              "requestor_id" => max_headroom,
              "permission" => 'read',
              "type" => 'actor',
              "collection" => [hasselhoff]
            }
            post("/bulk", :superuser,
              :payload => request).should have_status_code(400).
              with_body({"error" => "supplied ID 'deadbeefdeadbeefdeadbeefdeadbeef' does not exist"})
          end

          it "invalid requestor returns 400" do
            request = {
              "requestor_id" => 'nobody',
              "permission" => 'read',
              "type" => 'actor',
              "collection" => [hasselhoff]
            }
            post("/bulk", :superuser,
              :payload => request).should have_status_code(400).
              with_body({"error" => "invalid authz ID, must be 32-digit hex string"})
          end

          it "missing permission returns 400" do
            request = {
              "requestor_id" => shatner,
              "type" => 'actor',
              "collection" => [hasselhoff]
            }
            post("/bulk", :superuser,
              :payload => request).should have_status_code(400).
              with_body({"error" => "required request key 'permission' is missing"})
          end

          it "bad permission returns 400" do
            request = {
              "requestor_id" => shatner,
              "permission" => 'divine right',
              "type" => 'actor',
              "collection" => [hasselhoff]
            }
            post("/bulk", :superuser,
              :payload => request).should have_status_code(400).
              with_body({"error" => "invalid permission type, must be 'create', 'read', 'update', 'delete', or 'grant'"})
          end

          it "missing type returns 400" do
            request = {
              "requestor_id" => shatner,
              "permission" => 'read',
              "collection" => [hasselhoff]
            }
            post("/bulk", :superuser,
              :payload => request).should have_status_code(400).
              with_body({"error" => "required request key 'type' is missing"})
          end

          it "bad type returns 400" do
            request = {
              "requestor_id" => shatner,
              "permission" => 'read',
              "type" => 'bucket',
              "collection" => [hasselhoff]
            }
            post("/bulk", :superuser,
              :payload => request).should have_status_code(400).
              with_body({"error" => "invalid authz type, must be 'actor', 'container', 'group', or 'object'"})
          end

          it "missing collection returns 400" do
            request = {
              "requestor_id" => shatner,
              "permission" => 'read',
              "type" => 'actor'
            }
            post("/bulk", :superuser,
              :payload => request).should have_status_code(400).
              with_body({"error" => "required request key 'collection' is missing"})
          end

          it "bad collection returns 400" do
            request = {
              "requestor_id" => shatner,
              "permission" => 'read',
              "type" => 'actor',
              "collection" => 'summer'
            }
            post("/bulk", :superuser,
              :payload => request).should have_status_code(400).
              with_body({"error" => "request key 'collection' is incorrect JSON type"})
          end

          it "invalid member in collection returns 400" do
            request = {
              "requestor_id" => shatner,
              "permission" => 'read',
              "type" => 'actor',
              "collection" => ['fall']
            }
            post("/bulk", :superuser,
              :payload => request).should have_status_code(400).
              with_body({"error" => "invalid authz ID, must be 32-digit hex string"})
          end

          it "bogus key is ignored" do
            request = {
              "requestor_id" => shatner,
              "permission" => 'read',
              "type" => 'actor',
              "collection" => [hasselhoff],
              "bogus" => 'bogus'
            }
            post("/bulk", :superuser,
              :payload => request).should have_status_code(204)
          end

        end
      end

      [:actor, :group, :container, :object].each do |type|
        context "for #{type.upcase} type" do
          ['create', 'read', 'update', 'delete', 'grant'].each do |ace|

            context "for direct #{ace.upcase} permission" do
              with_actors :shatner, :hasselhoff
              with_entity type, :zuul
              with_entity type, :crystal

              with_ace_on :crystal, ace.to_sym, :to => :shatner

              it "entity with permission returns 204" do
                request = {
                  "requestor_id" => shatner,
                  "permission" => ace,
                  "type" => type,
                  "collection" => [crystal]
                  }
                post("/bulk", :shatner,
                  :payload => request).should have_status_code(204)
              end

              it "entity with different permission returns unauthorized" do
                req_ace = "create"
                if (ace == "create")
                  req_ace = "read"
                end
                request = {
                  "requestor_id" => shatner,
                  "permission" => req_ace,
                  "type" => type,
                  "collection" => [zuul]
                  }
                post("/bulk", :shatner,
                  :payload => request).should have_status_code(200).
                  with_body({"unauthorized" => [zuul]})
              end

              it "different entity without permission returns unauthorized" do
                request = {
                  "requestor_id" => shatner,
                  "permission" => ace,
                  "type" => type,
                  "collection" => [zuul]
                  }
                post("/bulk", :shatner,
                  :payload => request).should have_status_code(200).
                  with_body({"unauthorized" => [zuul]})
              end

              it "other actor without permission returns unauthorized" do
                request = {
                  "requestor_id" => hasselhoff,
                  "permission" => ace,
                  "type" => type,
                  "collection" => [crystal]
                  }
                post("/bulk", :shatner,
                  :payload => request).should have_status_code(200).
                  with_body({"unauthorized" => [crystal]})
              end

              it "fake entity returns unauthorized" do
                request = {
                  "requestor_id" => shatner,
                  "permission" => ace,
                  "type" => type,
                  "collection" => [easterbunny]
                  }
                post("/bulk", :shatner,
                  :payload => request).should have_status_code(200).
                  with_body({"unauthorized" => [easterbunny]})
              end

              it "list of actors returns only unauthorized actors" do
                request = {
                  "requestor_id" => shatner,
                  "permission" => ace,
                  "type" => type,
                  "collection" => [zuul, crystal, easterbunny]
                  }
                post("/bulk", :shatner,
                  :payload => request).should have_status_code(200).
                  with_body({"unauthorized" => [zuul, easterbunny]})
              end
            end

            context "with doubly-indirect permission" do
              with_actors :shatner, :hasselhoff
              with_group :hipsters, :members => [:shatner]
              with_group :brogrammers, :members => [:hipsters]
              with_entity type, :zuul
              with_entity type, :crystal

              with_ace_on :crystal, ace.to_sym, :to => :brogrammers

              it "entity with permission returns 204" do
                request = {
                  "requestor_id" => shatner,
                  "permission" => ace,
                  "type" => type,
                  "collection" => [crystal]
                  }
                post("/bulk", :shatner,
                  :payload => request).should have_status_code(204)
              end

              it "entity with different permission returns unauthorized" do
                req_ace = "create"
                if (ace == "create")
                  req_ace = "read"
                end
                request = {
                  "requestor_id" => shatner,
                  "permission" => req_ace,
                  "type" => type,
                  "collection" => [crystal]
                  }
                post("/bulk", :shatner,
                  :payload => request).should have_status_code(200).
                  with_body({"unauthorized" => [crystal]})
              end

              it "different entity without permission returns unauthorized" do
                request = {
                  "requestor_id" => shatner,
                  "permission" => ace,
                  "type" => type,
                  "collection" => [zuul]
                  }
                post("/bulk", :shatner,
                  :payload => request).should have_status_code(200).
                  with_body({"unauthorized" => [zuul]})
              end

              it "other actor without permission returns unauthorized" do
                request = {
                  "requestor_id" => hasselhoff,
                  "permission" => ace,
                  "type" => type,
                  "collection" => [crystal]
                  }
                post("/bulk", :shatner,
                  :payload => request).should have_status_code(200).
                  with_body({"unauthorized" => [crystal]})
              end

              it "fake entity returns unauthorized" do
                request = {
                  "requestor_id" => shatner,
                  "permission" => ace,
                  "type" => type,
                  "collection" => [easterbunny]
                  }
                post("/bulk", :shatner,
                  :payload => request).should have_status_code(200).
                  with_body({"unauthorized" => [easterbunny]})
              end

              it "list of actors returns only unauthorized actors" do
                request = {
                  "requestor_id" => shatner,
                  "permission" => ace,
                  "type" => type,
                  "collection" => [zuul, crystal, easterbunny]
                  }
                post("/bulk", :shatner,
                  :payload => request).should have_status_code(200).
                  with_body({"unauthorized" => [zuul, easterbunny]})
              end
            end

            context "with both direct and indirect permission" do
              with_actors :shatner, :hasselhoff
              with_group :hipsters, :members => [:shatner]
              with_entity type, :zuul
              with_entity type, :crystal

              with_ace_on :crystal, ace.to_sym, :to => :hipsters

              it "entity with permission returns 204" do
                request = {
                  "requestor_id" => shatner,
                  "permission" => ace,
                  "type" => type,
                  "collection" => [crystal]
                  }
                post("/bulk", :shatner,
                  :payload => request).should have_status_code(204)
              end

              it "entity with different permission returns unauthorized" do
                req_ace = "create"
                if (ace == "create")
                  req_ace = "read"
                end
                request = {
                  "requestor_id" => shatner,
                  "permission" => req_ace,
                  "type" => type,
                  "collection" => [zuul]
                  }
                post("/bulk", :shatner,
                  :payload => request).should have_status_code(200).
                  with_body({"unauthorized" => [zuul]})
              end

              it "different entity without permission returns unauthorized" do
                request = {
                  "requestor_id" => shatner,
                  "permission" => ace,
                  "type" => type,
                  "collection" => [zuul]
                  }
                post("/bulk", :shatner,
                  :payload => request).should have_status_code(200).
                  with_body({"unauthorized" => [zuul]})
              end

              it "other actor without permission returns unauthorized" do
                request = {
                  "requestor_id" => hasselhoff,
                  "permission" => ace,
                  "type" => type,
                  "collection" => [crystal]
                  }
                post("/bulk", :shatner,
                  :payload => request).should have_status_code(200).
                  with_body({"unauthorized" => [crystal]})
              end

              it "fake entity returns unauthorized" do
                request = {
                  "requestor_id" => shatner,
                  "permission" => ace,
                  "type" => type,
                  "collection" => [easterbunny]
                  }
                post("/bulk", :shatner,
                  :payload => request).should have_status_code(200).
                  with_body({"unauthorized" => [easterbunny]})
              end

              it "list of actors returns only unauthorized actors" do
                request = {
                  "requestor_id" => shatner,
                  "permission" => ace,
                  "type" => type,
                  "collection" => [zuul, crystal, easterbunny]
                  }
                post("/bulk", :shatner,
                  :payload => request).should have_status_code(200).
                  with_body({"unauthorized" => [zuul, easterbunny]})
              end
            end
          end
        end
      end
    end # POST

    should_not_allow :GET, "/bulk"
    should_not_allow :PUT, "/bulk"
    should_not_allow :DELETE, "/bulk"
  end # /bulk
end
