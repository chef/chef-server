require 'pedant/rspec/common'

describe "ACL API" do
  context "/<type>/<name>/_acl endpoint" do

    # TODO: Sanity check: users don't seem to have any ACLs, or at least, nothing is
    # accessible from external API as far as I can tell:
    ["clients", "groups", "containers", "data", "nodes", "roles", "environments",
      "cookbooks"].each do |type|
      context "for #{type} type" do
        let(:new_object) { "new-object" }
        let(:creation_url) { api_url(type) }
        let(:deletion_url) { api_url("#{type}/#{new_object}") }
        let(:request_url) { api_url("#{type}/#{new_object}/_acl") }

        # Because clients need to be created by superuser, but everything else shouldn't:
        let(:setup_user) { platform.admin_user }

        # Body used to create object (generally overriden):
        let(:creation_body) {{
            "name" => new_object
          }}

        # Yeah, this is confusing as hell, but for whatever insane reason the
        # default ACLs are different on almost every different types -- so these are
        # the defaults of defaults, which are overridden below for the different
        # types:
        let(:actors) { ["pivotal", "pedant_admin_user"] }
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
          let(:setup_user) { platform.superuser }
          let(:actors) { ["pivotal", new_object] }
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
          let(:actors) { ["pedant_admin_user"] }
          let(:groups) { [] }
          let(:grant_groups) { [] }
        when "data"
          let(:groups) { ["users", "clients", "admins"] }
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
            # Bloody inconsistent API needs a PUT here
            put(creation_url, setup_user,
              :payload => creation_body).should look_like({
                :status => 200
              })
          else
            post(creation_url, setup_user,
              :payload => creation_body).should look_like({
                :status => 201
              })
          end
        end

        after :each do
          delete(deletion_url, setup_user).should look_like({
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

          context "normal user" do
            it "returns 403" do
              get(request_url, platform.non_admin_user).should look_like({
                  :status => 403
                })
            end
          end

          context "client" do
            it "returns 403" do
              get(request_url, platform.non_admin_client).should look_like({
                  :status => 403
                })
            end
          end

          context "outside user" do
            it "returns 403" do
              get(request_url, outside_user).should look_like({
                  :status => 403
                })
            end
          end

          context "invalid user" do
            it "returns 401" do
              get(request_url, invalid_user).should look_like({
                  :status => 401
                })
            end
          end

          ["create", "read", "update", "delete", "grant"].each do |perm|
            context "when normal user granted #{perm}" do
              it "can get object ACL" do
                put("#{request_url}/#{perm}", platform.admin_user,
                  :payload => {perm => {
                      "actors" => [platform.non_admin_user.name,
                        platform.admin_user.name, "pivotal"],
                      "groups" => ["admins"]
                    }}).should look_like({
                    :status => 200
                  })
                get(request_url, platform.admin_user).should look_like({
                    :status => 200
                  })
              end
            end
          end
        end # context GET /<type>/<name>/_acl

        context "PUT /#{type}/<name>/_acl" do
          context "admin user" do
            # A 405 here would be fine (better, even)
            it "returns 404" do
              put(request_url, platform.admin_user).should look_like({
                  :status => 404
                })
            end
          end
        end # context PUT /<type>/<name>/_acl

        context "POST /#{type}/<name>/_acl" do
          context "admin user" do
            # A 405 here would be fine (better, even)
            it "returns 404" do
              post(request_url, platform.admin_user).should look_like({
                  :status => 404
                })
            end
          end
        end # context POST /<type>/<name>/_acl

        context "DELETE /#{type}/<name>/_acl" do
          context "admin user" do
            # A 405 here would be fine (better, even)
            it "returns 404" do
              delete(request_url, platform.admin_user).should look_like({
                  :status => 404
                })
            end
          end
        end # context DELETE /<type>/<name>/_acl

        ["create", "read", "update", "delete", "grant"].each do |permission|
          context "/#{type}/<name>/_acl/#{permission} endpoint" do

            let(:permission_request_url) { "#{request_url}/#{permission}" }

            context "GET /#{type}/<name>/_acl/#{permission}" do
              context "admin user" do
                # A 405 here would be fine (better, even)
                it "returns 404" do
                  get(permission_request_url, platform.admin_user).should look_like({
                      :status => 404
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

              context "normal user" do
                it "returns 403" do
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

              context "client" do
                it "returns 403" do
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
                it "returns 403" do
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
                it "returns 401" do
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

              context "normal user with all permissions except grant" do
                it "returns 403" do
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

              context "normal user with grant permission" do
                it "can update ACL" do
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
            end # context PUT /<type>/<name>/_acl/<permission>

            context "POST /#{type}/<name>/_acl/#{permission}" do
              context "admin user" do
                # A 405 here would be fine (better, even)
                it "returns 404" do
                  post(permission_request_url, platform.admin_user).should look_like({
                      :status => 404
                    })
                end
              end
            end # context POST /<type>/<name>/_acl/<permission>

            context "DELETE /#{type}/<name>/_acl/#{permission}" do
              context "admin user" do
                # A 405 here would be fine (better, even)
                it "returns 404" do
                  delete(permission_request_url, platform.admin_user).should look_like({
                      :status => 404
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
