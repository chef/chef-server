require 'pedant/rspec/common'
require 'pedant/acl'

describe "ACL API", :acl do
  include Pedant::ACL

  # Generate random string identifier prefixed with current pid
  def rand_id
    "#{Process.pid}_#{rand(10**7...10**8).to_s}"
  end

  let(:server_admins) { "::server-admins" }
  let(:other_admins) { "#{@test_orgname2}::admins" }
  let(:nonlocal_groups) { [server_admins, other_admins ] }

  before(:all) do
    @test_orgname2 = "test-org-#{rand_id}"
    platform.create_org(@test_orgname2) if Pedant.config[:org][:create_me]
  end

  after(:all) do
    platform.delete_org(@test_orgname2) if Pedant.config[:org][:create_me]
  end

  context "/users/<name>/_acl endpoint" do
    let(:username) { platform.admin_user.name }
    let(:request_url) { "#{platform.server}/users/#{username}/_acl" }

    let(:read_access_group) { "::" + platform.test_org.name + "_read_access_group"}
    let(:read_groups) { [read_access_group, server_admins] }
    let(:grant_groups) { [] }

    context "GET /users/<user>/_acl",  :chef_zero_quirks do

      let(:actors) { ["pivotal", username].uniq }
      let(:groups) { [ server_admins ] }

      let(:acl_body) {{
          "create" => {"actors" => actors, "groups" => groups},
          "read" => {"actors" => actors, "groups" => read_groups},
          "update" => {"actors" => actors, "groups" => groups},
          "delete" => {"actors" => actors, "groups" => groups},
          "grant" => {"actors" => actors, "groups" => grant_groups}
        }}

      context "superuser" do
        it "can get user acl" do
          get(request_url, platform.superuser).should look_like({
             :status => 200,
             :body => acl_body
          })
        end
      end
      context "admin user" do
        it "can get user acl" do
          get(request_url, platform.admin_user).should look_like({
             :status => 200,
             :body => acl_body
          })
        end
      end
    end

    %w(create read update delete grant).each do |permission|
      context "/users/<user>/_acl/#{permission} endpoint",  :chef_zero_quirks do
        if (permission == "read")
          smoketest = :smoke
        else
          smoketest = :notsmoke
        end
        let(:acl_url) { "#{platform.server}/users/#{username}/_acl" }
        let(:request_url)  { "#{platform.server}/users/#{username}/_acl/#{permission}" }
        context "PUT /users/<user>/_acl/#{permission}" do
          let(:actors) { ["pivotal", username].uniq }
          let(:groups) { [] }
          let(:default_body) {{
              "create" => {"actors" => actors, "groups" => groups},
              "read" => {"actors" => actors, "groups" => read_groups},
              "update" => {"actors" => actors, "groups" => groups},
              "delete" => {"actors" => actors, "groups" => groups},
              "grant" => {"actors" => actors, "groups" => groups}
            }}

          let(:request_body) {{
              permission => {
                "actors" => ["pivotal", platform.admin_user.name,
                             platform.non_admin_user.name],
                "groups" => groups
              }
            }}

          after :each do
            reset_body = {permission => default_body[permission]}
            put(request_url, platform.admin_user,
                :payload => reset_body).should look_like({
                                                           :status => 200
                                                         })
            # Make sure everything's the same again -- this could really screw up the
            # rest of the test suite if the permissions aren't right
            get(acl_url, platform.admin_user).should look_like({
                                                                 :status => 200,
                                                                 :body => default_body
                                                               })
          end

          context "admin user", smoketest do
            it "can modify ACL" do
              put(request_url, platform.admin_user,
                  :payload => request_body).should look_like({
                                                               :status => 200
                                                             })
              modified_body = default_body.dup;
              modified_body[permission] = request_body[permission]
              get(acl_url, platform.admin_user).should look_like({
                                                                   :status => 200,
                                                                   :body => modified_body
                                                                 })
            end
          end

          context "default normal user", smoketest do
            it "returns 403", :authorization do
              put(request_url, platform.non_admin_user,
                  :payload => request_body).should look_like({
                                                               :status => 403
                                                             })
              get(acl_url, platform.admin_user).should look_like({
                                                                   :status => 200,
                                                                   :body => default_body
                                                                 })
            end
          end

          context "default normal client" do
            it "returns 401", :authentication do
              put(request_url, platform.non_admin_client,
                  :payload => request_body).should look_like({
                                                               :status => 401
                                                             })
              get(acl_url, platform.admin_user).should look_like({
                                                                   :status => 200,
                                                                   :body => default_body
                                                                 })
            end
          end

          context "outside user" do
            it "returns 403", :authorization do
              put(request_url, outside_user,
                  :payload => request_body).should look_like({
                                                               :status => 403
                                                             })
              get(acl_url, platform.admin_user).should look_like({
                                                                   :status => 200,
                                                                   :body => default_body
                                                                 })
            end
          end

          context "invalid user" do
            it "returns 401", :authentication do
              put(request_url, invalid_user,
                  :payload => request_body).should look_like({
                                                               :status => 401
                                                             })
              get(acl_url, platform.admin_user).should look_like({
                                                                   :status => 200,
                                                                   :body => default_body
                                                                 })
            end
          end

          #
          # Nonexistent users are just dropped (perhaps this should be a 400, to match
          # organizations/<object>/_acl
          context "malformed requests" do
            context "invalid actor", :validation do
              let(:request_body) {{
                  permission => {
                    "actors" => ["pivotal", "bogus", platform.admin_user.name],
                    "groups" => permission == "read" ? read_groups : groups
                  }
                }}

              it "returns 400" do
                put(request_url, platform.admin_user,
                    :payload => request_body).should look_like({
                                                                 :status => 400
                                                               })
                get(acl_url, platform.admin_user).should look_like({
                                                                     :status => 200,
                                                                     :body => default_body
                                                                   })
              end
            end

            context "invalid group", :validation do
              let(:request_body) {{
                  permission => {
                    "actors" => ["pivotal", platform.admin_user.name,
                                 platform.non_admin_user.name],
                    "groups" => ["admins", "bogus"]
                  }
                }}

              it "returns 400", :validation do
                put(request_url, platform.admin_user,
                    :payload => request_body).should look_like({
                                                                 :status => 400
                                                               })
                get(acl_url, platform.admin_user).should look_like({
                                                                     :status => 200,
                                                                     :body => default_body
                                                                   })
              end
            end

            context "missing actors", :validation do
              let(:request_body) {{
                  permission => {
                    "groups" => groups
                  }
                }}

              it "returns 400", :validation do
                put(request_url, platform.admin_user,
                    :payload => request_body).should look_like({
                                                                 :status => 400
                                                               })
                get(acl_url, platform.admin_user).should look_like({
                                                                     :status => 200,
                                                                     :body => default_body
                                                                   })
              end
            end

            context "missing groups", :validation do
              let(:request_body) {{
                  permission => {
                    "actors" => ["pivotal", "bogus", platform.admin_user.name,
                                 platform.non_admin_user.name]
                  }
                }}

              it "returns 400", :validation do
                put(request_url, platform.admin_user,
                    :payload => request_body).should look_like({
                                                                 :status => 400
                                                               })
                get(acl_url, platform.admin_user).should look_like({
                                                                     :status => 200,
                                                                     :body => default_body
                                                                   })
              end
            end

            context "empty body", :validation do
              let(:request_body) { {} }

              it "returns 400", :validation do
                put(request_url, platform.admin_user,
                    :payload => request_body).should look_like({
                                                                 :status => 400
                                                               })
                get(acl_url, platform.admin_user).should look_like({
                                                                     :status => 200,
                                                                     :body => default_body
                                                                   })
              end
            end
          end # context malformed requests
        end

      end

      #
      # There's a clause 'with modified acls' for organizations objects below that should be extended
      # here, but some of the semantics around what they should be are unclear to me
      #

    end
  end

  context "/organizations/_acl endpoint" do
    let(:request_url) { api_url("organizations/_acl") }

    context "GET /organizations/_acl" do
      let(:actors) { ["pivotal"] }
      let(:groups) { ["admins"] }
      let(:read_groups) { ["admins", "users"] }
      let(:acl_body) {{
          "create" => {"actors" => actors, "groups" => groups},
          "read" => {"actors" => actors, "groups" => read_groups},
          "update" => {"actors" => actors, "groups" => groups},
          "delete" => {"actors" => actors, "groups" => groups},
          "grant" => {"actors" => actors, "groups" => groups}
        }}

      context "admin user" do
        it "can get object ACL" do
          get(request_url, platform.admin_user).should look_like({
              :status => 200,
              :body => acl_body
            })
        end
      end

      context "default normal user" do
        it "returns 403", :authorization do
          get(request_url, platform.non_admin_user).should look_like({
              :status => 403
            })
        end
      end

      context "default client" do
        it "returns 403", :authorization do
          get(request_url, platform.non_admin_client).should look_like({
              :status => 403
            })
        end
      end

      context "outside user" do
        it "returns 403", :authorization do
          get(request_url, outside_user).should look_like({
              :status => 403
            })
        end
      end

      context "invalid user" do
        it "returns 401", :authentication do
          get(request_url, invalid_user).should look_like({
              :status => 401
            })
        end
      end

      context "with modified ACLs" do
        after :each do
          %w(create read update delete grant).each do |perm|
            reset_body = { perm => acl_body[perm] }
            put("#{request_url}/#{perm}", superuser,
              :payload => reset_body).should look_like({
                :status => 200
              })
          end
          # Make sure everything's the same again -- this could really screw up the
          # rest of the test suite if the permissions aren't right -- in the long
          # run this is an obvious candidate for moving to a different org when we
          # support multi-org tests.
          get(request_url, platform.admin_user).should look_like({
              :status => 200,
              :body => acl_body
            })
        end

        context "when normal user granted all permissions except GRANT" do
          it "returns 403", :authorization do
            restrict_permissions_to("organizations",
              platform.non_admin_user => ['create', 'read', 'update', 'delete'])
            get(request_url, platform.non_admin_user).should look_like({
                :status => 403
              })
          end
        end

        context "when normal client granted all permissions except GRANT" do
          it "returns 403", :authorization, :smoke do
            restrict_permissions_to("organizations",
              platform.non_admin_client => ['create', 'read', 'update', 'delete'])
            get(request_url, platform.non_admin_client).should look_like({
                :status => 403
              })
          end
        end

        context "when normal user granted GRANT permission" do
          it "can get ACL" do
            restrict_permissions_to("organizations",
              platform.non_admin_user => ['grant'])
            get(request_url, platform.non_admin_user).should look_like({
                :status => 200
              })
          end
        end

        context "when normal client granted GRANT permission" do
          it "can get ACL", :smoke do
            restrict_permissions_to("organizations",
              platform.non_admin_client => ['grant'])
            get(request_url, platform.non_admin_client).should look_like({
                :status => 200
              })
          end
        end
      end # context with modified ACLs
    end # context GET /organizations/_acl

    context "PUT /organizations/_acl" do
      context "admin user" do
        it "returns 405" do
          put(request_url, platform.admin_user).should look_like({
              :status => 405
            })
        end
      end
    end # context PUT /organizations/_acl

    context "POST /organizations/_acl" do
      context "admin user" do
        it "returns 405" do
          post(request_url, platform.admin_user).should look_like({
              :status => 405
            })
        end
      end
    end # context POST /organizations/_acl

    context "DELETE /organizations/_acl" do
      context "admin user" do
        it "returns 405" do
          delete(request_url, platform.admin_user).should look_like({
              :status => 405
            })
        end
      end
    end # context DELETE /organizations/_acl
  end # context /organizations/_acl endpoint

  %w(create read update delete grant).each do |permission|
    context "/organizations/_acl/#{permission} endpoint" do
      # Don't run a smoke test test for every permission (to keep the smoke test count
      # from being unnecessarily repetetive)
      if (permission == "read")
        smoketest = :smoke
      else
        smoketest = :notsmoke
      end

      let(:acl_url) { api_url("organizations/_acl") }
      let(:request_url) { api_url("organizations/_acl/#{permission}") }

      context "PUT /organizations/_acl/#{permission}" do
        let(:actors) { ["pivotal"] }
        let(:groups) { ["admins"] }
        let(:read_groups) { ["admins", "users"] }
        let(:default_body) {{
            "create" => {"actors" => actors, "groups" => groups},
            "read" => {"actors" => actors, "groups" => read_groups},
            "update" => {"actors" => actors, "groups" => groups},
            "delete" => {"actors" => actors, "groups" => groups},
            "grant" => {"actors" => actors, "groups" => groups}
          }}

        let(:request_body) {{
            permission => {
              "actors" => ["pivotal", platform.admin_user.name,
                           platform.non_admin_user.name].uniq,
              "groups" => groups + nonlocal_groups
            }
          }}


        after :each do
          reset_body = {permission => default_body[permission]}
          put(request_url, platform.admin_user,
            :payload => reset_body).should look_like({
              :status => 200
            })
          # Make sure everything's the same again -- this could really screw up the
          # rest of the test suite if the permissions aren't right
          get(acl_url, platform.admin_user).should look_like({
              :status => 200,
              :body => default_body
            })
        end

        context "admin user" do
          it "can modify ACL" do
            put(request_url, platform.admin_user,
              :payload => request_body).should look_like({
                :status => 200
              })
            modified_body = default_body.dup;
            modified_body[permission] = request_body[permission]
            get(acl_url, platform.admin_user).should look_like({
                :status => 200,
                :body => modified_body
              })
          end
        end

        context "default normal user" do
          it "returns 403", :authorization do
            put(request_url, platform.non_admin_user,
              :payload => request_body).should look_like({
                :status => 403
              })
            get(acl_url, platform.admin_user).should look_like({
                :status => 200,
                :body => default_body
              })
          end
        end

        context "default normal client" do
          it "returns 403", :authorization do
            put(request_url, platform.non_admin_client,
              :payload => request_body).should look_like({
                :status => 403
              })
            get(acl_url, platform.admin_user).should look_like({
                :status => 200,
                :body => default_body
              })
          end
        end

        context "outside user" do
          it "returns 403", :authorization do
            put(request_url, outside_user,
              :payload => request_body).should look_like({
                :status => 403
              })
            get(acl_url, platform.admin_user).should look_like({
                :status => 200,
                :body => default_body
              })
          end
        end

        context "invalid user" do
          it "returns 401", :authentication do
            put(request_url, invalid_user,
              :payload => request_body).should look_like({
                :status => 401
              })
            get(acl_url, platform.admin_user).should look_like({
                :status => 200,
                :body => default_body
              })
          end
        end

        context "malformed requests" do
          context "invalid actor" do
            let(:request_body) {{
                permission => {
                  "actors" => ["pivotal", "bogus", platform.admin_user.name,
                    platform.non_admin_user.name],
                  "groups" => groups
                }
              }}

            it "returns 400", :validation do
              put(request_url, platform.admin_user,
                :payload => request_body).should look_like({
                  :status => 400
                })
              get(acl_url, platform.admin_user).should look_like({
                  :status => 200,
                  :body => default_body
                })
            end
          end

          context "invalid group (organization not found)" do
            let(:test_org_not_exist) { "test-org-#{rand_id}" }
            let(:admins_from_org_not_exist) {
              "#{test_org_not_exist}::admins"
            }
            let(:groups_with_org_not_exist) { ["admins", server_admins, admins_from_org_not_exist ] }
            let(:request_body) {{
                permission => {
                  "actors" => ["pivotal", platform.admin_user.name,
                    platform.non_admin_user.name],
                  "groups" => groups_with_org_not_exist,
                }
              }}

            it "returns 400", :validation do
              put(request_url, platform.admin_user,
                :payload => request_body).should look_like({
                  :status => 400
                })
              get(acl_url, platform.admin_user).should look_like({
                  :status => 200,
                  :body => default_body
                })
            end
          end

          context "invalid group (group not found)" do
            let(:request_body) {{
                permission => {
                  "actors" => ["pivotal", platform.admin_user.name,
                    platform.non_admin_user.name],
                  "groups" => ["admins", "bogus"]
                }
              }}

            it "returns 400", :validation do
              put(request_url, platform.admin_user,
                :payload => request_body).should look_like({
                  :status => 400
                })
              get(acl_url, platform.admin_user).should look_like({
                  :status => 200,
                  :body => default_body
                })
            end
          end

          context "missing actors" do
            let(:request_body) {{
                permission => {
                  "groups" => groups
                }
              }}

            it "returns 400", :validation do
              put(request_url, platform.admin_user,
                :payload => request_body).should look_like({
                  :status => 400
                })
              get(acl_url, platform.admin_user).should look_like({
                  :status => 200,
                  :body => default_body
                })
            end
          end

          context "missing groups" do
            let(:request_body) {{
                permission => {
                  "actors" => ["pivotal", "bogus", platform.admin_user.name,
                    platform.non_admin_user.name]
                }
              }}

            it "returns 400", :validation do
              put(request_url, platform.admin_user,
                :payload => request_body).should look_like({
                  :status => 400
                })
              get(acl_url, platform.admin_user).should look_like({
                  :status => 200,
                  :body => default_body
                })
            end
          end

          context "empty body" do
            let(:request_body) { {} }

            it "returns 400", :validation do
              put(request_url, platform.admin_user,
                :payload => request_body).should look_like({
                  :status => 400
                })
              get(acl_url, platform.admin_user).should look_like({
                  :status => 200,
                  :body => default_body
                })
            end
          end
        end # context malformed requests

        context "with modified ACLs" do
          after :each do
            %w(create read update delete grant).each do |perm|
                reset_body = { perm => default_body[perm] }
              put("#{acl_url}/#{perm}", superuser,
                :payload => reset_body).should look_like({
                  :status => 200
                })
            end
            # Make sure everything's the same again -- this could really screw up the
            # rest of the test suite if the permissions aren't right
            get(acl_url, platform.admin_user).should look_like({
                :status => 200,
                :body => default_body
              })
          end

          context "when normal user granted all permissions except GRANT" do
            # We only run the smoke tests for read permission (set above)
            it "returns 403", :authorization do
              restrict_permissions_to("organizations",
                platform.non_admin_user => ['create', 'read', 'update', 'delete'])
              put(request_url, platform.non_admin_user,
                :payload => request_body).should look_like({
                  :status => 403
                })
            end
          end

          context "when normal client granted all permissions except GRANT" do
            # We only run the smoke tests for read permission (set above)
            it "returns 403", :authorization do
              restrict_permissions_to("organizations",
                platform.non_admin_client => ['create', 'read', 'update', 'delete'])
              put(request_url, platform.non_admin_client,
                :payload => request_body).should look_like({
                  :status => 403
                })
            end
          end

          context "when normal user granted GRANT permission" do
            # We only run the smoke tests for read permission (set above)
            it "can modify ACL", smoketest do
              restrict_permissions_to("organizations",
                platform.non_admin_user => ['grant'])
              put(request_url, platform.non_admin_user,
                :payload => request_body).should look_like({
                  :status => 200
                })
            end
          end

          context "when normal client granted GRANT permission" do
            # We only run the smoke tests for read permission (set above)
            it "can modify ACL" do
              restrict_permissions_to("organizations",
                platform.non_admin_client => ['grant'])
              put(request_url, platform.non_admin_client,
                :payload => request_body).should look_like({
                  :status => 200
                })
            end
          end
        end # context with modified ACLs
      end # context PUT /organizations/_acl/<permission>

      context "GET /organizations/_acl/#{permission}" do
        context "admin user" do
          it "returns 405" do
            get(request_url, platform.admin_user).should look_like({
                :status => 405
              })
          end
        end
      end # context GET /organizations/_acl/<permission>

      context "POST /organizations/_acl/#{permission}" do
        context "admin user" do
          it "returns 405" do
            post(request_url, platform.admin_user).should look_like({
                :status => 405
              })
          end
        end
      end # context POST /organizations/_acl/<permission>

      context "DELETE /organizations/_acl/#{permission}" do
        context "admin user" do
          it "returns 405" do
            delete(request_url, platform.admin_user).should look_like({
                :status => 405
              })
          end
        end
      end # context DELETE /organizations/_acl/<permission>
    end # context /organizations/_acl/<permission> endpoint
  end # loop (each) over permissions

  # Special case that doesn't fit into the generic behaviors above - specifically
  # when a client & a user of the same name exist, updates to an acl specifying
  # the common name as the actor should fail with a 422, because we have no
  # way of knowing if the caller wanted to add the client or the user to the ACL.
  context "when a client exists with the same name as a user", :validation do
    let(:admin_requestor){admin_user}
    let(:requestor){admin_requestor}
    let(:shared_name) { "pedant-acl-#{rand_id}" }
    let(:request_url) { api_url("/clients/#{shared_name}/_acl/read") }
    let(:acl_request_body) {
      { read: { actors: ['pivotal', shared_name],
                         groups: ['admins'] } }
    }

    before :each do
      @user = platform.create_user(shared_name, associate: false)
      @client = platform.create_client(shared_name, platform.test_org)
    end

    after :each do
      platform.delete_user(@user)
      platform.delete_client(@client)
    end

    context "and the user is a member of the organization" do
      before :each do
        platform.associate_user_with_org(platform.test_org.name, @user)
      end

      it "updates of the object ACL results in a 422 due to ambiguous request" do
        expect(put(request_url, platform.admin_user, payload: acl_request_body)).
          to look_like(status: 422)
      end

      context "and 'clients' and 'users' fields are provided in the request" do
        let(:acl_request_body) {
          { read: { "actors" => [],
                    "users" => ['pivotal', shared_name ],
                    "clients" => [ shared_name ],
                    "groups" => ['admins'] } }
        }

        it "updates of the object ACL using 'clients' and 'users' are successful" do
          expect(put(request_url, platform.admin_user, payload: acl_request_body))
            .to have_status_code 200

          # Verify that the returned list contains this actor twice (once
          # as client and once as user), since we don't separate them in the GET.
          res = get(api_url("/clients/#{shared_name}/_acl"), platform.admin_user)
          read_ace = JSON.parse(res.body)['read']
          expect(read_ace['actors'].sort).to eq [shared_name, shared_name, "pivotal"]
        end
      end
    end

    context "and the user is not a member of the organization" do
      it "there is no ambiguity and the object ACL update succeeds" do
        expect(put(request_url, platform.admin_user, payload: acl_request_body))
          .to have_status_code 200

      end
    end
  end

  context "/<type>/<name>/_acl endpoint" do

    # TODO: Sanity check: users don't seem to have any ACLs, or at least, nothing is
    # accessible from external API as far as I can tell:
    # - [jkeiser] Users have ACLs, but they are at /users/NAME/_acl
    %w(clients groups containers data nodes roles environments cookbooks policies policy_groups).each do |type|
      context "for #{type} type" do

        let(:new_object) { "new-object" }
        let(:creation_url) { api_url(type) }
        let(:deletion_url) { api_url("#{type}/#{new_object}") }
        let(:request_url) { api_url("#{type}/#{new_object}/_acl") }

        let(:setup_user) { platform.admin_user }

        # Body used to create object (generally overriden):
        let(:creation_body) {{
            "name" => new_object
          }}

        # Yeah, this is confusing as hell, but for whatever insane reason the
        # default ACLs are different on almost every different types -- so these are
        # the defaults of defaults, which are overridden below for the different
        # types:
        let(:actors) { ["pivotal", platform.admin_user.name].uniq }
        let(:users) { ["pivotal", platform.admin_user.name].uniq }
        let(:clients) { [] }
        let(:groups) { ["admins"] }
        let(:read_groups) { groups }
        let(:update_groups) { groups }
        let(:delete_groups) { groups }
        # Usually still ["admins"] even when the other groups aren't:
        let(:grant_groups) { ["admins"] }
        let(:acl_body) {{
            "create" => {"actors" => actors, "groups" => groups},
            "read" => {"actors" => actors, "groups" => read_groups},
            "update" => {"actors" => actors, "groups" => update_groups},
            "delete" => {"actors" => actors, "groups" => delete_groups},
            "grant" => {"actors" => actors, "groups" => grant_groups}
          }}

        let(:granular_acl_body) {{
          "create" => {"actors" => [], "users" => users, "clients" => clients, "groups" => groups},
          "read" => {"actors" => [], "users" => users, "clients" => clients, "groups" => read_groups},
          "update" => {"actors" => [], "users" => users, "clients" => clients, "groups" => update_groups},
          "delete" => {"actors" => [], "users" => users, "clients" => clients, "groups" => delete_groups},
          "grant" => {"actors" => [], "users" => users, "clients" => clients, "groups" => grant_groups},
        }}

        # Mainly this is for the different creation bodies, but also for the
        # different default ACLs for each type, etc.  We love consistency!
        case type
        when "clients"
          let(:actors) {
            # As long as 'new_object' isn't a validator (and you're on
            # the Erchef client endpoint), new_object will be in the
            # actors list
            ["pivotal", new_object, setup_user.name].uniq
          }
          let(:clients) { [ new_object ] }
          let(:users) { ["pivotal", setup_user.name].uniq }

          let(:read_groups) { ["users", "admins"] }
          let(:delete_groups) { ["users", "admins"] }
        when "groups"
          let(:creation_body) {{
              "id" => new_object
            }}
          let(:read_groups) { ["users", "admins"] }
        when "containers"
          let(:creation_body) {{
              "id" => new_object,
              "containerpath" => "/"
            }}
          let(:actors) { [platform.admin_user.name] }
          let(:users) {  [platform.admin_user.name] }
          let(:groups) { [] }
          let(:grant_groups) { [] }
        when "data"
          let(:read_groups) { ["users","clients", "admins"] }
          let(:groups) { ["users","admins"] }
        when "nodes"
          let(:groups) { ["users", "clients", "admins"] }
          let(:update_groups) { ["users", "admins"] }
          let(:delete_groups) { ["users", "admins"] }
        when "roles"
          let(:creation_body) {{
              "name" => new_object,
              "json_class" => "Chef::Role"
            }}
          let(:groups) { ["users", "admins"] }
          let(:read_groups) { ["users", "clients", "admins"] }
       when "environments"
          let(:creation_body) {{
              "name" => new_object,
              "json_class" => "Chef::Environment"
            }}
          let(:groups) { ["users", "admins"] }
          let(:read_groups) { ["users", "clients", "admins"] }
        when "cookbooks"
          let(:version) { "1.0.0" }
          let(:creation_url) { api_url("#{type}/#{new_object}/#{version}") }
          let(:deletion_url) { creation_url }
          let(:creation_body) {{
              "name" => "#{new_object}-#{version}",
              "cookbook_name" => new_object,
              "version" => version,
              "json_class" => "Chef::CookbookVersion",
              "chef_type" => "cookbook_version",
              "frozen?" => false,
              "recipes" => [],
              "metadata" => {
                "version" => version,
                "name" => new_object,
                "maintainer" => "spacemonkey",
                "maintainer_email" => "spacemonkey@chef.io",
                "description" => "",
                "long_description" => "",
                "license" => "",
                "dependencies" => {},
                "attributes" => {},
                "recipes" => {}
              }
            }}
          let(:groups) { ["users", "admins"] }
          let(:read_groups) { ["users", "clients", "admins"] }
        when "policies"
          let(:creation_url) { api_url("#{type}/#{new_object}/revisions") }
          let(:creation_body) {{
            "revision_id" => "909c26701e291510eacdc6c06d626b9fa5350d25",
            "name" => new_object,
            "run_list" => [ "recipe[policyfile_demo::default]" ],
            "cookbook_locks" => {
              "policyfile_demo" => {
                "identifier" => "f04cc40faf628253fe7d9566d66a1733fb1afbe9",
                "version" => "1.2.3"
              }
            }
          }}
          let(:groups) { ["users", "admins"] }
          let(:read_groups) { ["users", "clients", "admins"] }
        when "policy_groups"
          let(:creation_url) {
            api_url("#{type}/#{new_object}/policies/acl_test_policy")
          }
          let(:creation_body) {{
            "revision_id" => "909c26701e291510eacdc6c06d626b9fa5350d25",
            "name" => "acl_test_policy",
            "run_list" => [ "recipe[policyfile_demo::default]" ],
            "cookbook_locks" => {
              "policyfile_demo" => {
                "identifier" => "f04cc40faf628253fe7d9566d66a1733fb1afbe9",
                "version" => "1.2.3"
              }
            }
          }}
          let(:groups) { ["users", "admins"] }
          let(:read_groups) { ["users", "clients", "admins"] }
        end

        before :each do
          if (type == "cookbooks" || type == "policy_groups")
            # Inconsistent API needs a PUT here.  We love consistency!
            put(creation_url, setup_user,
              :payload => creation_body).should look_like({
                :status => 201
              })
          else
            post(creation_url, setup_user,
              :payload => creation_body).should look_like({
                :status => 201
              })
          end
        end

        after :each do
          if (type == "policy_groups")
            # Policy groups are only created indirectly when we create policies;
            # deleting the group doesn't delete the policy so we do it explicitly
            delete(api_url("policies/acl_test_policy"),
              platform.admin_user).should look_like({
                :status => 200
              })
          end
          delete(deletion_url, platform.admin_user).should look_like({
              :status => 200
            })
        end

        context "GET /#{type}/<name>/_acl" do
          context "admin user" do
            it "can get object ACL" do
              get(request_url, platform.admin_user).should look_like({
                  :status => 200,
                  :body_exact => acl_body
                })
            end
            it "can get a granular object ACL" do
              get("#{request_url}?detail=granular", platform.admin_user).should look_like({
                  :status => 200,
                  :body_exact => granular_acl_body
                })

            end
          end

          context "default normal user" do
            it "returns 403", :authorization do
              get(request_url, platform.non_admin_user).should look_like({
                  :status => 403
                })
            end
          end

          context "default client" do
            it "returns 403", :authorization do
              get(request_url, platform.non_admin_client).should look_like({
                  :status => 403
                })
            end
          end

          context "outside user" do
            it "returns 403", :authorization do
              get(request_url, outside_user).should look_like({
                  :status => 403
                })
            end
          end

          context "invalid user" do
            it "returns 401", :authentication do
              get(request_url, invalid_user).should look_like({
                  :status => 401
                })
            end
          end

          context "when normal user granted all permissions except GRANT" do
            it "returns 403", :authorization do
              ["create", "read", "update", "delete"].each do |perm|
                put("#{request_url}/#{perm}", platform.admin_user,
                  :payload => {perm => {
                      "actors" => [platform.non_admin_user.name,
                        platform.admin_user.name, "pivotal"],
                      "groups" => ["admins"]
                    }}).should look_like({
                    :status => 200
                  })
              end
              get(request_url, platform.non_admin_user).should look_like({
                  :status => 403
                })
            end
          end

          context "when normal client granted all permissions except GRANT" do
            it "returns 403", :authorization do
              ["create", "read", "update", "delete"].each do |perm|
                put("#{request_url}/#{perm}", platform.admin_user,
                  :payload => {perm => {
                      "actors" => [platform.non_admin_client.name,
                        platform.admin_user.name, "pivotal"],
                      "groups" => ["admins"]
                    }}).should look_like({
                    :status => 200
                  })
              end
              get(request_url, platform.non_admin_client).should look_like({
                  :status => 403
                })
            end
          end

          context "when normal user granted GRANT permission" do
            it "can get object ACL" do
              put("#{request_url}/grant", platform.admin_user,
                :payload => {"grant" => {
                    "actors" => [platform.non_admin_user.name,
                      platform.admin_user.name, "pivotal"],
                    "groups" => ["admins"]
                  }}).should look_like({
                  :status => 200
                })
              get(request_url, platform.non_admin_user).should look_like({
                  :status => 200
                })
            end
          end

          context "when normal client granted GRANT permission" do
            it "can get object ACL" do
              put("#{request_url}/grant", platform.admin_user,
                :payload => {"grant" => {
                    "actors" => [platform.non_admin_client.name,
                      platform.admin_user.name, "pivotal"],
                    "groups" => ["admins"]
                  }}).should look_like({
                  :status => 200
                })
              get(request_url, platform.non_admin_client).should look_like({
                  :status => 200
                })
            end
          end

          context "OC-1702 - when containing a missing group" do
            let(:missing_group)  { "missing-group" }
            let(:updated_read)   { {"actors" => actors, "groups" => [missing_group] + read_groups} }

            before(:each) do
              # create the groups
              post(api_url("groups"), platform.admin_user,
                    :payload => {"id" => missing_group}).should look_like({:status => 201})

              # add the group to the read ace
              put("#{request_url}/read",
                  platform.admin_user,
                  :payload => { "read" => updated_read }).should look_like(:status => 200)

              # delete the group
              delete(api_url("groups/#{missing_group}"), platform.admin_user)
            end

            after(:each) do
              delete(api_url("groups/#{missing_group}"), platform.admin_user)
            end

            it "should return the acl", :validation do
              get(request_url, platform.admin_user).should look_like({
                :status => 200,
                :body => acl_body
              })
            end
          end
        end # context GET /<type>/<name>/_acl

        context "PUT /#{type}/<name>/_acl" do
          context "admin user" do
            it "returns 405" do
              put(request_url, platform.admin_user).should look_like({
                  :status => 405
                })
            end
          end
        end # context PUT /<type>/<name>/_acl

        context "POST /#{type}/<name>/_acl" do
          context "admin user" do
            it "returns 405" do
              post(request_url, platform.admin_user).should look_like({
                  :status => 405
                })
            end
          end
        end # context POST /<type>/<name>/_acl

        context "DELETE /#{type}/<name>/_acl" do
          context "admin user" do
            it "returns 405" do
              delete(request_url, platform.admin_user).should look_like({
                  :status => 405
                })
            end
          end
        end # context DELETE /<type>/<name>/_acl

        %w(create read update delete grant).each do |permission|
          context "/#{type}/<name>/_acl/#{permission} endpoint" do
            # Don't run a smoke test test for every permission (to keep the smoke
            # test count from being unnecessarily repetetive). Also avoid minor
            # overlap with somewhat similar group and container tests
            if (permission == "update" && type != "groups" && type != "containers")
              smoketest = :smoke
            else
              smoketest = :nosmoke
            end

            let(:permission_request_url) { "#{request_url}/#{permission}" }

            context "GET /#{type}/<name>/_acl/#{permission}" do
              context "admin user" do
                it "returns 405" do
                  get(permission_request_url, platform.admin_user).should look_like({
                      :status => 405
                    })
                end
              end
            end # context GET /<type>/<name>/_acl/<permission>

            context "PUT /#{type}/<name>/_acl/#{permission}" do
              before(:all) do
                @client_with_dot = platform.create_client("test-client.#{rand_id}")
              end

              after(:all) do
                platform.delete_client(@client_with_dot)
              end


              let(:clients) { [platform.non_admin_client.name] }
              let(:users) {
                [platform.non_admin_user.name, platform.admin_user.name, "pivotal"].uniq
              }
              let(:local_groups) { ["admins", "users", "clients"] }

              let(:groups_and_actors) {{
                  "actors" => [platform.non_admin_user.name,
                               platform.admin_user.name, "pivotal"].uniq + clients,
                  "groups" => local_groups
                }}
              let(:update_body) {{
                  permission => groups_and_actors
                }}

              context "admin user" do
                context "using the new 'users' and 'clients' attributes" do
                  let(:update_body) {
                    { permission => {
                      "actors" => [], # back-compat, empty actors and
                                      # clients/users present indicates
                                      # that clients/users should be used.
                      "users" => users,
                      "clients" => clients,
                      "groups" => local_groups
                      }
                    }
                  }
                  let(:response_body) {
                    { permission =>  groups_and_actors }
                  }

                  it "can update ACL" do
                    put(permission_request_url, platform.admin_user,
                        :payload => update_body).should have_status_code 200
                    # Note that resulting GET body should look the same -
                    # we are not returning clients/users separately at this point
                    # to avoid a confusing response that includes both 'actors'
                    # and 'clients/users', when we only accept one of those options containing
                    # values. If we revisit and determine it's needed,
                    # it will be a new APIv2 behavior.
                    check_body = acl_body
                    check_body[permission] = groups_and_actors

                    get(request_url, platform.admin_user).should look_like({
                        :status => 200,
                        :body => check_body
                        })
                  end
                end

                context "using the legacy 'actors' attribute" do
                  let(:update_body) { { permission => groups_and_actors } }
                  it "can update ACL" do
                    put(permission_request_url, platform.admin_user,
                      :payload => update_body).should look_like({
                        :status => 200
                      })
                    check_body = acl_body
                    check_body[permission] = groups_and_actors

                    get(request_url, platform.admin_user).should look_like({
                        :status => 200,
                        :body => check_body
                      })
                  end

                  context "when client with '.' in name is used in actors hash" do
                    let(:groups_and_actors) do
                      {
                        "actors" => [@client_with_dot.name, platform.admin_user.name, "pivotal"],
                        "groups" => ["admins"]
                      }
                    end

                    let(:update_body) do
                      { permission => groups_and_actors }
                    end

                    it "returns 200", :acl do
                      put("#{request_url}/#{permission}", platform.admin_user, :payload => update_body).should look_like({:status => 200})

                      check_body = acl_body
                      check_body[permission] = groups_and_actors
                      get(request_url, platform.admin_user).should look_like({:status => 200, :body => check_body})
                    end
                  end
                end

                # Note that we don't test every single permutation of the API with non local groups
                # to avoid too much combinatorial explosion. However this should be sufficient for now.
                context "with non-local groups" do
                   let(:update_body) {
                    { permission => {
                      "actors" => users + clients,
                      "groups" => local_groups + nonlocal_groups
                      }
                    }
                   }
                   let(:extended_groups_and_actors) {
                     x = groups_and_actors.dup
                     x["groups"] = local_groups + nonlocal_groups
                   }

                  it "can update ACL"  do
                    put(permission_request_url, platform.admin_user,
                        :payload => update_body).should have_status_code 200
                    check_body = acl_body.dup
                    check_body[permission] = update_body[permission]

                    get(request_url, platform.admin_user).should look_like({
                        :status => 200,
                        :body => check_body
                        })
                  end


                end
              end

              context "default normal user" do
                it "returns 403", :authorization do
                  put(permission_request_url, platform.non_admin_user,
                    :payload => update_body).should look_like({
                      :status => 403
                    })
                  get(request_url, platform.admin_user).should look_like({
                      :status => 200,
                      :body => acl_body
                    })
                end
              end

              context "default client" do
                it "returns 403", :authorization do
                  put(permission_request_url, platform.non_admin_client,
                    :payload => update_body).should look_like({
                      :status => 403
                    })
                  get(request_url, platform.admin_user).should look_like({
                      :status => 200,
                      :body => acl_body
                    })
                end
              end

              context "outside user" do
                it "returns 403", :authorization do
                  put(permission_request_url, outside_user,
                    :payload => update_body).should look_like({
                      :status => 403
                    })
                  get(request_url, platform.admin_user).should look_like({
                      :status => 200,
                      :body => acl_body
                    })
                end
              end

              context "invalid user" do
                it "returns 401", :authentication do
                  put(permission_request_url, invalid_user,
                    :payload => update_body).should look_like({
                      :status => 401
                    })
                  get(request_url, platform.admin_user).should look_like({
                      :status => 200,
                      :body => acl_body
                    })
                end
              end

              context "malformed requests", :validation do
                context "invalid actor" do
                  let(:update_body) {{
                      permission => {
                        "actors" => ["pivotal", "bogus", platform.admin_user.name,
                          platform.non_admin_user.name],
                        "groups" => ["admins", "users", "clients"]
                      }
                    }}

                  it "returns 400", :validation do
                    put(permission_request_url, platform.admin_user,
                      :payload => update_body).should look_like({
                        :status => 400
                      })
                    get(request_url, platform.admin_user).should look_like({
                        :status => 200,
                        :body => acl_body
                      })
                  end
                end
                context "includes valid actor list and valid client list" do
                  let(:update_body) {
                    {
                      permission => {
                        "actors" => ["pivotal", platform.admin_user.name,
                          platform.non_admin_user.name],
                        "clients" =>  [platform.non_admin_client.name],
                        "groups" => ["admins", "users", "clients"]
                      }
                    }
                  }

                  it "returns 400", :validation do
                    response = put(permission_request_url, platform.admin_user,
                      :payload => update_body)
                    expect(response).to have_status_code 400
                  end
                end

                context "includes valid actor list and valid user list" do
                  let(:update_body) {
                    {
                      permission => {
                        "actors" => ["pivotal", platform.admin_user.name,
                          platform.non_admin_user.name],
                        "users" =>  ["pivotal"],
                        "groups" => ["admins", "users", "clients"]
                      }
                    }
                  }

                  it "returns 400", :validation do
                    response = put(permission_request_url, platform.admin_user,
                      :payload => update_body)
                    expect(response).to have_status_code 400
                  end
                end

                context "includes valid actor list and valid user and client lists" do
                  let(:update_body) {
                    {
                      permission => {
                        "actors" => ["pivotal", platform.admin_user.name,
                          platform.non_admin_user.name],
                        "users" =>  ["pivotal"],
                        "clients" => [ platform.non_admin_client.name ],
                        "groups" => ["admins", "users", "clients"]
                      }
                    }
                  }

                  it "returns 400", :validation do
                    response = put(permission_request_url, platform.admin_user,
                      :payload => update_body)
                    expect(response).to have_status_code 400
                  end
                end

                context "invalid client" do
                  let(:update_body) {{
                      permission => {
                        "actors" => [],
                        "users" => ["pivotal", platform.admin_user.name,
                          platform.non_admin_user.name],
                        "clients" => [platform.non_admin_client.name, "bogus"],
                        "groups" => ["admins", "users", "clients"]
                      }
                    }}

                  it "returns 400", :validation do
                    response = put(permission_request_url, platform.admin_user,
                      :payload => update_body)
                    expect(response).to have_status_code 400
                    get(request_url, platform.admin_user).should look_like({
                        :status => 200,
                        :body => acl_body
                      })
                  end
                end

                context "invalid group" do
                  let(:update_body) {{
                      permission => {
                        "actors" => ["pivotal", platform.admin_user.name,
                          platform.non_admin_user.name],
                        "groups" => ["admins", "users", "clients", "bogus"]
                      }
                    }}

                  it "returns 400", :validation do
                    put(permission_request_url, platform.admin_user,
                      :payload => update_body).should have_status_code 400
                    get(request_url, platform.admin_user).should look_like({
                        :status => 200,
                        :body => acl_body
                      })
                  end
                end

                context "missing actors" do
                  let(:update_body) {{
                      permission => {
                        "groups" => ["admins", "users", "clients"]
                      }
                    }}

                  it "returns 400", :validation do
                    put(permission_request_url, platform.admin_user,
                      :payload => update_body).should look_like({
                        :status => 400
                      })
                    get(request_url, platform.admin_user).should look_like({
                        :status => 200,
                        :body => acl_body
                      })
                  end
                end

                context "missing groups" do
                  let(:update_body) {{
                      permission => {
                        "actors" => ["pivotal", platform.admin_user.name,
                          platform.non_admin_user.name]
                      }
                    }}

                  it "returns 400", :validation do
                    put(permission_request_url, platform.admin_user,
                      :payload => update_body).should look_like({
                        :status => 400
                      })
                    get(request_url, platform.admin_user).should look_like({
                        :status => 200,
                        :body => acl_body
                      })
                  end
                end

                context "empty body" do
                  let(:update_body) { {} }

                  it "returns 400", :validation do
                    put(permission_request_url, platform.admin_user,
                      :payload => update_body).should look_like({
                        :status => 400
                      })
                    get(request_url, platform.admin_user).should look_like({
                        :status => 200,
                        :body => acl_body
                      })
                  end
                end
              end # context malformed requests

              context "normal user with all permissions except GRANT", :authorization do
                # We only run the smoke tests for read permission (set above)
                it "returns 403", smoketest do
                  ["create", "read", "update", "delete"].each do |perm|
                    put("#{request_url}/#{perm}", platform.admin_user,
                      :payload => {perm => {
                          "actors" => [platform.non_admin_user.name,
                            platform.admin_user.name, "pivotal"],
                          "groups" => []
                        }}).should look_like({
                        :status => 200
                      })
                  end

                  put(permission_request_url, platform.non_admin_user,
                    :payload => update_body).should look_like({
                      :status => 403
                    })
                end
              end

              context "normal user with GRANT permission" do
                # We only run the smoke tests for read permission (set above)
                it "can update ACL", smoketest do
                  put("#{request_url}/grant", platform.admin_user,
                    :payload => {"grant" => {
                        "actors" => [platform.non_admin_user.name,
                          platform.admin_user.name, "pivotal"],
                        # Per ACL policy, non-superusers can't remove the admins
                        # group from the grant ACL.
                        "groups" => ["admins"]
                      }}).should look_like({
                      :status => 200
                    })

                  put(permission_request_url, platform.non_admin_user,
                    :payload => update_body).should look_like({
                      :status => 200
                    })
                end
              end

              context "normal client with all permissions except GRANT", :authorization do
                it "returns 403", :authorization do
                  ["create", "read", "update", "delete"].each do |perm|
                    put("#{request_url}/#{perm}", platform.admin_user,
                      :payload => {perm => {
                          "actors" => [platform.non_admin_client.name,
                            platform.admin_user.name, "pivotal"],
                          "groups" => []
                        }}).should look_like({
                        :status => 200
                      })
                  end

                  put(permission_request_url, platform.non_admin_client,
                    :payload => update_body).should look_like({
                      :status => 403
                    })
                end
              end

              context "normal client with GRANT permission" do
                it "can update ACL" do
                  put("#{request_url}/grant", platform.admin_user,
                    :payload => {"grant" => {
                        "actors" => [platform.non_admin_client.name,
                          platform.admin_user.name, "pivotal"],
                        # Per ACL policy, non-superusers can't remove the admins
                        # group from the grant ACL.
                        "groups" => ["admins"]
                      }}).should look_like({
                      :status => 200
                    })

                  put(permission_request_url, platform.non_admin_client,
                    :payload => update_body).should look_like({
                      :status => 200
                    })
                end
              end
            end # context PUT /<type>/<name>/_acl/<permission>

            context "POST /#{type}/<name>/_acl/#{permission}" do
              context "admin user" do
                it "returns 405" do
                  post(permission_request_url, platform.admin_user).should look_like({
                      :status => 405
                    })
                end
              end
            end # context POST /<type>/<name>/_acl/<permission>

            context "DELETE /#{type}/<name>/_acl/#{permission}" do
              context "admin user" do
                it "returns 405" do
                  delete(permission_request_url, platform.admin_user).should look_like({
                      :status => 405
                    })
                end
              end
            end # context DELETE /<type>/<name>/_acl/<permission>
          end # context /<type>/<name>/_acl/<permission> endpoint
        end # loop (each) over permission
      end # context for <type> type
    end # loop (each) over type
  end # context /<type>/<name>/_acl endpoint
end # describe ACL API
