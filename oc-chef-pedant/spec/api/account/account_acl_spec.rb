require 'pedant/rspec/common'
require 'pedant/acl'

describe "ACL API", :acl do
  include Pedant::ACL

  # (temporarily?) deprecating /users/*/_acl endpoint due to its broken state and lack of usefulness
  skip "/users/<name>/_acl endpoint" do
    let(:username) { platform.admin_user.name }
    let(:request_url) { "#{platform.server}/users/#{username}/_acl" }

    let(:read_access_group) { platform.test_org.name + "_read_access_group"}
    let(:read_groups) { [read_access_group] }

    context "GET /users/<user>/_acl"  do

      let(:actors) { ["pivotal", username] }
      let(:groups) { [] }

      let(:acl_body) {{
          "create" => {"actors" => actors, "groups" => groups},
          "read" => {"actors" => actors, "groups" => read_groups},
          "update" => {"actors" => actors, "groups" => groups},
          "delete" => {"actors" => actors, "groups" => groups},
          "grant" => {"actors" => actors, "groups" => groups}
        }}

      context "superuser" do
        it "can get user acl" do
          get(request_url, platform.superuser).should look_like({
             :status => 200,
             :body_exact => acl_body
          })
        end
      end
      context "admin user" do
        it "can get user acl" do
          get(request_url, platform.admin_user).should look_like({
             :status => 200,
             :body_exact => acl_body
          })
        end
      end
    end


    %w(create read update delete grant).each do |permission|
      context "/users/<user>/_acl/#{permission} endpoint" do
        if (permission == "read")
          smoketest = :smoke
        else
          smoketest = :notsmoke
        end
        let(:acl_url) { "#{platform.server}/users/#{username}/_acl" }
        let(:request_url)  { "#{platform.server}/users/#{username}/_acl/#{permission}" }

        context "PUT /users/<user>/_acl/#{permission}" do
          let(:actors) { ["pivotal", username] }
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
                                                                 :body_exact => default_body
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
                                                                   :body_exact => modified_body
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
                                                                   :body_exact => default_body
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
                                                                   :body_exact => default_body
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
                                                                   :body_exact => default_body
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
                                                                   :body_exact => default_body
                                                                 })
            end
          end

          #
          # Nonexistent users are just dropped (perhaps this should be a 400, to match
          # organizations/<object>/_acl
          context "malformed requests" do
            context "invalid actor" do
              let(:request_body) {{
                  permission => {
                    "actors" => ["pivotal", "bogus", platform.admin_user.name],
                    "groups" => permission == "read" ? read_groups : groups
                  }
                }}
              it "returns 200" do
                put(request_url, platform.admin_user,
                    :payload => request_body).should look_like({
                                                                 :status => 200
                                                               })
                get(acl_url, platform.admin_user).should look_like({
                                                                     :status => 200,
                                                                     :body_exact => default_body
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
                                                                     :body_exact => default_body
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
                                                                     :body_exact => default_body
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
                                                                     :body_exact => default_body
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
                                                                     :body_exact => default_body
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

  context "/ANY/_acl" do
    let(:request_url) {api_url("ANY/_acl")}
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
    it "can get org acl by default" do
      get(request_url, platform.admin_user).should look_like({
              :status => 200,
              :body_exact => acl_body
            })
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
              :body_exact => acl_body
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
              :body_exact => acl_body
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
              :body_exact => default_body
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
                :body_exact => modified_body
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
                :body_exact => default_body
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
                :body_exact => default_body
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
                :body_exact => default_body
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
                :body_exact => default_body
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
                  :body_exact => default_body
                })
            end
          end

          context "invalid group" do
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
                  :body_exact => default_body
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
                  :body_exact => default_body
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
                  :body_exact => default_body
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
                  :body_exact => default_body
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
                :body_exact => default_body
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

  context "/<type>/<name>/_acl endpoint" do

    # TODO: Sanity check: users don't seem to have any ACLs, or at least, nothing is
    # accessible from external API as far as I can tell:
    # - [jkeiser] Users have ACLs, but they are at /users/NAME/_acl
    %w(clients groups containers data nodes roles environments cookbooks).each do |type|
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
        let(:actors) { ["pivotal", platform.admin_user.name] }
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

        # Mainly this is for the different creation bodies, but also for the
        # different default ACLs for each type, etc.  We love consistency!
        case type
        when "clients"
          let(:actors) {
            # As long as 'new_object' isn't a validator (and you're on
            # the Erchef client endpoint), new_object will be in the
            # actors list
            ["pivotal", new_object, setup_user.name]
          }
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
                "maintainer_email" => "spacemonkey@opscode.com",
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
        end

        before :each do
          if (type == "cookbooks")
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
                :body_exact => acl_body
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

              let(:groups_and_actors) {{
                  "actors" => [platform.non_admin_user.name,
                    platform.admin_user.name, "pivotal"],
                  "groups" => ["admins", "users", "clients"]
                }}
              let(:update_body) {{
                  permission => groups_and_actors
                }}

              context "admin user" do
                it "can update ACL" do
                  put(permission_request_url, platform.admin_user,
                    :payload => update_body).should look_like({
                      :status => 200
                    })
                  check_body = acl_body
                  check_body[permission] = groups_and_actors

                  get(request_url, platform.admin_user).should look_like({
                      :status => 200,
                      :body_exact => check_body
                    })
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
                      :body_exact => acl_body
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
                      :body_exact => acl_body
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
                      :body_exact => acl_body
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
                      :body_exact => acl_body
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
                        :body_exact => acl_body
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
                      :payload => update_body).should look_like({
                        :status => 400
                      })
                    get(request_url, platform.admin_user).should look_like({
                        :status => 200,
                        :body_exact => acl_body
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
                        :body_exact => acl_body
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
                        :body_exact => acl_body
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
                        :body_exact => acl_body
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
                        "groups" => []
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
                        "groups" => []
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
