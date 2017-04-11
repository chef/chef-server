# -*- coding: utf-8 -*-
require 'pedant/rspec/common'

describe "opscode-account groups", :groups do
  let(:org)        { platform.test_org.name }

  context "/groups endpoint" do
    let(:request_url) { api_url("groups") }
    context "GET /groups" do
      # This is only a partial body -- there are other groups as well, but these
      # should all exist for an organization:
      let(:default_groups) {{
          "admins" => "#{request_url}/admins",
          "billing-admins" => "#{request_url}/billing-admins",
          "clients" => "#{request_url}/clients",
          "users" => "#{request_url}/users"
        }}

      context "admin user" do
        it "can get groups", :smoke do
          get(request_url, platform.admin_user).should look_like({
              :status => 200,
              :body => default_groups
            })
        end
      end

      context "normal user" do
        it "can get groups" do
          get(request_url, platform.non_admin_user).should look_like({
              :status => 200,
              :body => default_groups
            })
        end
      end

      context "client" do
        # Is this actually right?  Seems like this should be 200
        it "returns 403", :authorization do
          get(request_url, platform.non_admin_client).should look_like({
              :status => 403
            })
        end
      end

      context "outside user" do
        it "returns 403", :authorization, :smoke do
          get(request_url, platform.bad_user).should look_like({
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
    end # GET /groups

    context "POST /groups" do
      let(:new_group) { "new-group" }

      let(:request_body) {{
          "groupname" => new_group
        }}

      let(:response_body) {{
          "uri" => "#{request_url}/#{new_group}"
        }}

      # This is only a partial body -- there are other groups as well, but these
      # should all exist for an organization:
      let(:list_of_groups_without_new_group) {{
          "admins" => "#{request_url}/admins",
          "billing-admins" => "#{request_url}/billing-admins",
          "clients" => "#{request_url}/clients",
          "users" => "#{request_url}/users"
        }}

      let(:list_of_groups_with_new_group) {{
          "admins" => "#{request_url}/admins",
          "billing-admins" => "#{request_url}/billing-admins",
          "clients" => "#{request_url}/clients",
          "users" => "#{request_url}/users",
          new_group => "#{request_url}\/#{new_group}",
        }}

      after :each do
        begin
          delete(api_url("/groups/#{new_group}"), platform.admin_user)
        rescue
          # Swallow errors attempting to delete the invalid groups we try to create;
          # those tests should either fail before they get here or never actually
          # create the groups
        end
      end

      context "permissions" do
        context "admin user" do
          it "can create group", :smoke do
            post(request_url, platform.admin_user,
              :payload => request_body).should look_like({
                :status => 201,
                :body_exact => response_body
              })
            get(request_url, platform.admin_user).should look_like({
                :status => 200,
                :body => list_of_groups_with_new_group
              })
            get("#{request_url}/#{new_group}", platform.admin_user).should look_like({
                :status => 200
              })
          end
        end

        context "normal user" do
          it "returns 403", :authorization do
            post(request_url, platform.non_admin_user,
              :payload => request_body).should look_like({
                :status => 403
              })
            get(request_url, platform.admin_user).should look_like({
                :status => 200,
                :body => list_of_groups_without_new_group
              })
            get("#{request_url}/#{new_group}", platform.admin_user).should look_like({
                :status => 404
              })
          end
        end

        context "client" do
          it "returns 403", :authorization do
            post(request_url, platform.non_admin_client,
              :payload => request_body).should look_like({
                :status => 403
              })
            get(request_url, platform.admin_user).should look_like({
                :status => 200,
                :body => list_of_groups_without_new_group
              })
            get("#{request_url}/#{new_group}", platform.admin_user).should look_like({
                :status => 404
              })
          end
        end

        context "outside user" do
          it "returns 403", :authorization, :smoke do
            post(request_url, outside_user,
              :payload => request_body).should look_like({
                :status => 403
              })
            get(request_url, platform.admin_user).should look_like({
                :status => 200,
                :body => list_of_groups_without_new_group
              })
            # TODO: shouldn't this properly be 403?
            get("#{request_url}/#{new_group}", platform.admin_user).should look_like({
                :status => 404
              })
          end
        end

        context "invalid user" do
          it "returns 401", :authentication do
            post(request_url, invalid_user,
              :payload => request_body).should look_like({
                :status => 401
              })
            get(request_url, platform.admin_user).should look_like({
                :status => 200,
                :body => list_of_groups_without_new_group
              })
            # TODO: Shouldn't this properly be 401?
            get("#{request_url}/#{new_group}", platform.admin_user).should look_like({
                :status => 404
              })
          end
        end
      end # context Permissions

      context "group creation" do
        context "when group already exists" do
          before :each do
            post(request_url, platform.admin_user,
              :payload => request_body).should look_like({
                :status => 201,
                :body_exact => response_body
              })
          end

          it "returns 409" do
            post(request_url, platform.admin_user,
              :payload => request_body).should look_like({
                :status => 409
              })
          end
        end

        context "with no group name" do
          let(:request_body) { {} }

          it "returns 400", :validation do
            post(request_url, platform.admin_user,
              :payload => request_body).should look_like({
                :status => 400
              })
            get(request_url, platform.admin_user).should look_like({
                :status => 200,
                :body => list_of_groups_without_new_group
              })
            get("#{request_url}/#{new_group}", platform.admin_user).should look_like({
                :status => 404
              })
          end
        end

        context "name instead of group name" do
          let(:request_body) {{
              "name" => new_group
            }}

          it "returns 400", :validation do
            post(request_url, platform.admin_user,
              :payload => request_body).should look_like({
                :status => 400
              })
            get(request_url, platform.admin_user).should look_like({
                :status => 200,
                :body => list_of_groups_without_new_group
              })
            get("#{request_url}/#{new_group}", platform.admin_user).should look_like({
                :status => 404
              })
          end
        end

        context "with id instead of group name" do
          let(:request_body) {{
              "id" => new_group
            }}

          it "can create group" do
            post(request_url, platform.admin_user,
              :payload => request_body).should look_like({
                :status => 201,
                :body_exact => response_body
              })
            get(request_url, platform.admin_user).should look_like({
                :status => 200,
                :body => list_of_groups_with_new_group
              })
            get("#{request_url}/#{new_group}", platform.admin_user).should look_like({
                :status => 200
              })
          end
        end

        context "with non-matching id and group name" do
          let(:request_body) {{
              "id" => new_group,
              "groupname" => "other"
            }}

          it "can create group (id wins)" do
            post(request_url, platform.admin_user,
              :payload => request_body).should look_like({
                :status => 201,
                :body_exact => response_body
              })
            get(request_url, platform.admin_user).should look_like({
                :status => 200,
                :body => list_of_groups_with_new_group
              })
            get("#{request_url}/#{new_group}", platform.admin_user).should look_like({
                :status => 200
              })
          end
        end

        context "with bogus value in request" do
          let(:request_body) {{
              "groupname" => new_group,
              "dude" => "sweet"
            }}

          it "can create group (ignores bogus value)" do
            post(request_url, platform.admin_user,
              :payload => request_body).should look_like({
                :status => 201,
                :body_exact => response_body
              })
            get(request_url, platform.admin_user).should look_like({
                :status => 200,
                :body => list_of_groups_with_new_group
              })
            get("#{request_url}/#{new_group}", platform.admin_user).should look_like({
                :status => 200
              })
          end
        end

        context "with empty group name" do
          let(:new_group) { "" }

          it "returns 400", :validation do
            post(request_url, platform.admin_user,
              :payload => request_body).should look_like({
                :status => 400
              })
            get(request_url, platform.admin_user).should look_like({
                :status => 200,
                :body => list_of_groups_without_new_group
              })
          end
        end

        context "with space in group name" do
          let(:new_group) { "new group" }

          it "returns 400", :validation do
            post(request_url, platform.admin_user,
              :payload => request_body).should look_like({
                :status => 400
              })
            get(request_url, platform.admin_user).should look_like({
                :status => 200,
                :body => list_of_groups_without_new_group
              })
          end
        end

        context "with unicode in group name" do
          let(:new_group) { "グループ" }

          it "can create group", :validation do
            skip "returns 400"
            post(request_url, platform.admin_user,
              :payload => request_body).should look_like({
                :status => 201,
                :body_exact => response_body
              })
            get(request_url, platform.admin_user).should look_like({
                :status => 200,
                :body => list_of_groups_with_new_group
              })
            get("#{request_url}/#{new_group}", platform.admin_user).should look_like({
                :status => 200
              })
          end
        end

        context "with users, clients, and groups" do
          let(:request_body) {{
              "groupname" => new_group,
              "users" => [platform.non_admin_user],
              "clients" => [platform.non_admin_client],
              "groups" => ["users"]
            }}

          let(:group_body) {{
              "actors" => [],
              "users" => [],
              "clients" => [],
              "groups" => [],
              "orgname" => org,
              "name" => new_group,
              "groupname" => new_group
            }}

          it "ignores them", :validation do
            post(request_url, platform.admin_user,
              :payload => request_body).should look_like({
                :status => 201,
                :body_exact => response_body
              })
            get(request_url, platform.admin_user).should look_like({
                :status => 200,
                :body => list_of_groups_with_new_group
              })
            get("#{request_url}/#{new_group}", platform.admin_user).should look_like({
                :status => 200,
                :body_exact => group_body
              })
          end
        end
      end # context group creation
    end # context POST /groups

    context "DELETE /groups" do
      context "admin user" do
        it "returns 405" do
          delete(request_url, platform.admin_user).should look_like({
              :status => 405
            })
        end
      end
    end

    context "PUT /groups" do
      context "admin user" do
        it "returns 405" do
          put(request_url, platform.admin_user).should look_like({
              :status => 405
            })
        end
      end
    end
  end # context /groups endpoint

  context "/groups/<name> endpoint" do
    let(:request_url) { api_url("groups/#{test_group}") }
    let(:test_group) { "test-group" }
    let(:test_orgname2) { "test-org-#{rand_id}-#{Process.pid}" }

    before :each do
      post(api_url("groups"), platform.admin_user,
        :payload => {"id" => test_group}).should look_like({:status => 201})
    end

    after :each do
      delete(request_url, platform.admin_user)
    end

    let(:default_group_body) {{
        "actors" => [],
        "users" => [],
        "clients" => [],
        "groups" => [],
        "orgname" => org,
        "name" => test_group,
        "groupname" => test_group
      }}

    context "GET /groups/<name>" do
      context "admin user" do
        it "can get group", :smoke do
          get(request_url, platform.admin_user).should look_like({
              :status => 200,
              :body_exact => default_group_body
            })
        end
      end

      context "normal user" do
        it "can get group" do
          get(request_url, platform.non_admin_user).should look_like({
              :status => 200,
              :body_exact => default_group_body
            })
        end
      end

      context "normal user without read ACE returns 403", :authorization do
        it "can't read group" do
          put(api_url("groups/#{test_group}/_acl/read"), platform.admin_user,
            :payload => {"read" => {
                "actors" => [platform.admin_user.name],
                "groups" => []
              }}).should look_like({
              :status => 200
            })

          get(request_url, platform.non_admin_user).should look_like({
              :status => 403
            })
        end
      end

      context "normal user without any ACE returns 403", :authorization do
        it "can't read group" do
          ["read", "grant", "update", "create", "delete"].each do |permission|
            put(api_url("groups/#{test_group}/_acl/#{permission}"), platform.admin_user,
              :payload => {permission => {
                  "actors" => [platform.admin_user.name],
                  # Leave admins in the groups, as per ACL policy
                  # it can't be removed from the grant ACL except by the superuser
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

      context "client" do
        # Is this actually right?  Seems like this should be 200
        it "returns 403", :authorization do
          get(request_url, platform.non_admin_client).should look_like({
              :status => 403
            })
        end
      end

      context "outside user", :smoke do
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

      context "OC-11702 - when a containing a missing group" do
        let(:missing_group) { "missing-group" }

        before(:each) do
          post(api_url("groups"), platform.admin_user,
                :payload => {"id" => missing_group}).should look_like({:status => 201})
          platform.add_group_to_group(org, missing_group, test_group, platform.admin_user)
          delete(api_url("groups/#{missing_group}"), platform.admin_user)
        end

        it "can get group", :validation do
          get(request_url, platform.admin_user).should look_like({
              :status => 200,
              :body_exact => default_group_body
            })
        end
      end
    end # context GET /groups/<name>

    context "DELETE /groups/<name>" do
      context "admin user" do
        it "can delete group", :smoke do
          delete(request_url, platform.admin_user).should look_like({
              :status => 200
            })
          get(request_url, platform.admin_user).should look_like({
              :status => 404
            })
        end
      end

      context "normal user" do
        it "returns 403", :authorization do
          delete(request_url, platform.non_admin_user).should look_like({
              :status => 403
            })
          get(request_url, platform.admin_user).should look_like({
              :status => 200
            })
        end
      end

      context "client" do
        # Is this actually right?  Seems like this should be 200
        it "returns 403", :authorization do
          delete(request_url, platform.non_admin_client).should look_like({
              :status => 403
            })
          get(request_url, platform.admin_user).should look_like({
              :status => 200
            })
        end
      end

      context "outside user", :smoke do
        it "returns 403", :authorization do
          delete(request_url, outside_user).should look_like({
              :status => 403
            })
          get(request_url, platform.admin_user).should look_like({
              :status => 200
            })
        end
      end

      context "invalid user" do
        it "returns 401", :authentication do
          delete(request_url, invalid_user).should look_like({
              :status => 401
            })
          get(request_url, platform.admin_user).should look_like({
              :status => 200
            })
        end
      end
    end # context DELETE /groups/<name>

    context "PUT /groups/<name>" do
      context "permissions" do
        let(:new_group_payload) {{
            "groupname" => test_group,
            "actors" => {"clients" => [platform.non_admin_client.name],
                         "users" => [platform.non_admin_user.name],
                         "groups" => ["users"]
                        },
          }}

        let(:modified_group_body) {{
            "actors" => [platform.non_admin_user.name, platform.non_admin_client.name],
            "users" => [platform.non_admin_user.name],
            "clients" => [platform.non_admin_client.name],
            "groups" => ["users"],
            "orgname" => org,
            "name" => test_group,
            "groupname" => test_group
          }}

        context "admin user" do
          it "can update group" do
            put(request_url, platform.admin_user,
              :payload => new_group_payload).should look_like({
                :status => 200
              })
            get(request_url, platform.admin_user).should look_like({
                :status => 200,
                :body_exact => modified_group_body
              })
          end
        end

        let(:server_admins) {
          "::server-admins"
        }
        let(:other_admins) {
          "#{test_orgname2}::admins"
        }
        let(:new_group_payload_with_global) {{
          "groupname" => test_group,
          "actors" => {"clients" => [platform.non_admin_client.name],
                       "users" => [platform.non_admin_user.name],
                       "groups" => ["users", server_admins, other_admins]
                      },
        }}
        let(:modified_group_body_with_global) {{
            "actors" => [platform.non_admin_user.name, platform.non_admin_client.name],
            "users" => [platform.non_admin_user.name],
            "clients" => [platform.non_admin_client.name],
            "groups" => ["users", server_admins, other_admins],
            "orgname" => org,
            "name" => test_group,
            "groupname" => test_group
        }}

        context "admin user can reference global group and other orgs" do
          it "can update group" do
            put(request_url, platform.admin_user,
                :payload => new_group_payload_with_global).should look_like({
                :status => 200
              })
            get(request_url, platform.admin_user).should look_like({
                :status => 200,
                :body_exact => modified_group_body_with_global
            })
          end
        end


        context "admin user cannot remove self from group" do
          let(:initial_group_payload) {{
              "groupname" => test_group,
              "actors" => {"clients" => [platform.non_admin_client.name],
                "users" => [platform.admin_user.name],
                "groups" => ["users"]}
            }}

          let(:initial_group_body) {{
              "actors" => [platform.admin_user.name, platform.non_admin_client.name],
              "users" => [platform.admin_user.name],
              "clients" => [platform.non_admin_client.name],
              "groups" => ["users"],
              "orgname" => org,
              "name" => test_group,
              "groupname" => test_group
            }}

          it "can update group" do
            skip "pending discussion" do
              # Created this test to check for this, but it has been decided to
              # table this for now; it's uncertain under what conditions we
              # would prevent users from removing themselves (or perhaps the
              # last user in a group), so this may not be used at all
              put(request_url, platform.admin_user,
                :payload => initial_group_payload).should look_like({
                  :status => 200
                })
              get(request_url, platform.admin_user).should look_like({
                  :status => 200,
                  :body_exact => initial_group_body
                })
              put(request_url, platform.admin_user,
                :payload => new_group_payload).should look_like({
                  :status => 400
                })
              get(request_url, platform.admin_user).should look_like({
                  :status => 200,
                  :body_exact => initial_group_body
                })
            end
          end
        end

        context "normal user with update ACE" do
          it "can update group", :smoke do
            put(api_url("groups/#{test_group}/_acl/update"), platform.admin_user,
              :payload => {"update" => {
                  "actors" => [platform.non_admin_user.name,
                    platform.admin_user.name, "pivotal"],
                  "groups" => ["admins"]
                }}).should look_like({
                :status => 200
              })

            put(request_url, platform.non_admin_user,
              :payload => new_group_payload).should look_like({
                :status => 200
              })
            get(request_url, platform.admin_user).should look_like({
                :status => 200,
                :body_exact => modified_group_body
              })
          end
        end

        context "normal user with update ACE cannot remove self" do
          let(:removed_from_group_payload) {{
              "groupname" => test_group,
              "actors" => {"clients" => [platform.non_admin_client.name],
                "users" => [platform.admin_user.name],
                "groups" => []}
            }}

          it "can update group" do
            skip "pending discussion" do
              # Created this test to check for this, but it has been decided to
              # table this for now; it's uncertain under what conditions we
              # would prevent users from removing themselves (or perhaps the
              # last user in a group), so this may not be used at all
              put(api_url("groups/#{test_group}/_acl/update"), platform.admin_user,
                :payload => {"update" => {
                    "actors" => [platform.non_admin_user.name,
                      platform.admin_user.name, "pivotal"],
                    "groups" => ["admins"]
                  }}).should look_like({
                  :status => 200
                })

              put(request_url, platform.non_admin_user,
                :payload => new_group_payload).should look_like({
                  :status => 200
                })
              get(request_url, platform.admin_user).should look_like({
                  :status => 200,
                  :body_exact => modified_group_body
                })
              put(request_url, platform.non_admin_user,
                :payload => removed_from_group_payload).should look_like({
                  :status => 400
                })
              get(request_url, platform.admin_user).should look_like({
                  :status => 200,
                  :body_exact => modified_group_body
                })
            end
          end
        end

        context "normal user without update ACE" do
          it "returns 403", :authorization, :smoke do
            put(request_url, platform.non_admin_user,
              :payload => new_group_payload).should look_like({
                :status => 403
              })
            get(request_url, platform.admin_user).should look_like({
                :status => 200,
                :body_exact => default_group_body
              })
          end
        end

        context "client" do
          # Is this actually right?  Seems like this should be 200
          it "returns 403", :authorization do
            put(request_url, platform.non_admin_client,
              :payload => new_group_payload).should look_like({
                :status => 403
              })
            get(request_url, platform.admin_user).should look_like({
                :status => 200,
                :body_exact => default_group_body
              })
          end
        end

        context "outside user" do
          it "returns 403", :authorization do
            put(request_url, outside_user,
              :payload => new_group_payload).should look_like({
                :status => 403
              })
            get(request_url, platform.admin_user).should look_like({
                :status => 200,
                :body_exact => default_group_body
              })
          end
        end

        context "invalid user" do
          it "returns 401", :authentication do
            put(request_url, invalid_user,
              :payload => new_group_payload).should look_like({
                :status => 401
              })
            get(request_url, platform.admin_user).should look_like({
                :status => 200,
                :body_exact => default_group_body
              })
          end
        end
      end # context permissions

      context "updating groups" do
        let(:new_group_payload) {{
            "groupname" => test_group,
            "actors" => {"clients" => [platform.non_admin_client.name],
              "users" => [platform.non_admin_user.name],
              "groups" => ["users"]}
          }}

        let(:modified_group_body) {{
            "actors" => [platform.non_admin_user.name, platform.non_admin_client.name],
            "users" => [platform.non_admin_user.name],
            "clients" => [platform.non_admin_client.name],
            "groups" => ["users"],
            "orgname" => org,
            "name" => test_group,
            "groupname" => test_group
          }}

        context "with different group name" do
          let(:new_group_name) { "new-group" }

          let(:new_group_payload) {{
              "groupname" => new_group_name,
              "actors" => {"clients" => [platform.non_admin_client.name],
                "users" => [platform.non_admin_user.name],
                "groups" => ["users"]}
            }}

          let(:modified_group_body) {{
              "actors" => [platform.non_admin_user.name, platform.non_admin_client.name],
              "users" => [platform.non_admin_user.name],
              "clients" => [platform.non_admin_client.name],
              "groups" => ["users"],
              "orgname" => org,
              "name" => new_group_name,
              "groupname" => new_group_name
            }}

          after :each do
            delete(api_url("groups/#{new_group_name}"), platform.admin_user)
          end

          it "will rename group" do
            put(request_url, platform.admin_user,
              :payload => new_group_payload).should look_like({
                :status => 201
              })
            get(request_url, platform.admin_user).should look_like({
                :status => 404
              })
            get(api_url("groups/#{new_group_name}"),
              platform.admin_user).should look_like({
                :status => 200,
                :body_exact => modified_group_body
              })
          end

          it "will not overwrite existing group" do
            post(api_url("groups"), platform.admin_user,
              :payload => {"id" => new_group_name}).should look_like({:status => 201})

            put(request_url, platform.admin_user,
              :payload => new_group_payload).should look_like({
                :status => 409
              })
            get(request_url, platform.admin_user).should look_like({
                :status => 200,
                :body_exact => default_group_body
              })
            get(api_url("groups/#{new_group_name}"),
              platform.admin_user).should look_like({
                :status => 200
              })
          end
        end # context with different group name

        context "with only user change" do
          let(:new_group_payload) {{
              "groupname" => test_group,
              "actors" => {"users" => [platform.non_admin_user.name]}
            }}

          let(:modified_group_body) {{
              "actors" => [platform.non_admin_user.name],
              "users" => [platform.non_admin_user.name],
              "clients" => [],
              "groups" => [],
              "orgname" => org,
              "name" => test_group,
              "groupname" => test_group
            }}

          it "will update group" do
            put(request_url, platform.admin_user,
              :payload => new_group_payload).should look_like({
                :status => 200
              })
            get(request_url, platform.admin_user).should look_like({
                :status => 200,
                :body_exact => modified_group_body
              })
          end
        end # context with only user change

        context "with only client change" do
          let(:new_group_payload) {{
              "groupname" => test_group,
              "actors" => {"clients" => [platform.non_admin_client.name]}
            }}

          let(:modified_group_body) {{
              "actors" => [platform.non_admin_client.name],
              "users" => [],
              "clients" => [platform.non_admin_client.name],
              "groups" => [],
              "orgname" => org,
              "name" => test_group,
              "groupname" => test_group
            }}

          it "will update group" do
            put(request_url, platform.admin_user,
              :payload => new_group_payload).should look_like({
                :status => 200
              })
            get(request_url, platform.admin_user).should look_like({
                :status => 200,
                :body_exact => modified_group_body
              })
          end
        end # context with only client change

        context "with only group change" do
          let(:new_group_payload) {{
              "groupname" => test_group,
              "actors" => {"groups" => ["users"]}
            }}

          let(:modified_group_body) {{
              "actors" => [],
              "users" => [],
              "clients" => [],
              "groups" => ["users"],
              "orgname" => org,
              "name" => test_group,
              "groupname" => test_group
            }}

          it "will update group" do
            put(request_url, platform.admin_user,
              :payload => new_group_payload).should look_like({
                :status => 200
              })
            get(request_url, platform.admin_user).should look_like({
                :status => 200,
                :body_exact => modified_group_body
              })
          end
        end # context with only group change

        context "without groupname" do
          let(:new_group_payload) {{
              "actors" => {"clients" => [platform.non_admin_client.name],
                "users" => [platform.non_admin_user.name],
                "groups" => ["users"]}
            }}

          it "will update group" do
            put(request_url, platform.admin_user,
              :payload => new_group_payload).should look_like({
                :status => 200
              })
            get(request_url, platform.admin_user).should look_like({
                :status => 200,
                :body_exact => modified_group_body
              })
          end
        end # context without groupname

        context "with bogus id instead of groupname", :validation do
          let(:new_group_payload) {{
              "id" => "foo",
              "actors" => {"clients" => [platform.non_admin_client.name],
                "users" => [platform.non_admin_user.name],
                "groups" => ["users"]}
            }}

          it "will update group" do
            put(request_url, platform.admin_user,
              :payload => new_group_payload).should look_like({
                :status => 200
              })
            get(request_url, platform.admin_user).should look_like({
                :status => 200,
                :body_exact => modified_group_body
              })
          end
        end # context with bogus id instead of groupname

        context "with random bogus value", :validation do
          let(:new_group_payload) {{
              "bogus" => "random",
              "actors" => {"clients" => [platform.non_admin_client.name],
                "users" => [platform.non_admin_user.name],
                "groups" => ["users"]}
            }}

          it "will update group" do
            put(request_url, platform.admin_user,
              :payload => new_group_payload).should look_like({
                :status => 200
              })
            get(request_url, platform.admin_user).should look_like({
                :status => 200,
                :body_exact => modified_group_body
              })
          end
        end # context with random bogus value

        context "with empty groupname" do
          let(:new_group_payload) {{
              "groupname" => "",
              "actors" => {"clients" => [platform.non_admin_client.name],
                "users" => [platform.non_admin_user.name],
                "groups" => ["users"]}
            }}

          it "returns 400", :validation do
            put(request_url, platform.admin_user,
              :payload => new_group_payload).should look_like({
                :status => 400
              })
            get(request_url, platform.admin_user).should look_like({
                :status => 200,
                :body_exact => default_group_body
              })
          end
        end # context with empty groupname

        context "with bogus client" do
          let(:new_group_payload) {{
              "groupname" => test_group,
              "actors" => {"clients" => ["bogus"],
                "users" => [platform.non_admin_user.name],
                "groups" => ["users"]}
            }}

          it "returns 400", :validation do
            skip "returns 500 instead" do
              put(request_url, platform.admin_user,
                :payload => new_group_payload).should look_like({
                  :status => 400
                })
              get(request_url, platform.admin_user).should look_like({
                  :status => 200,
                  :body_exact => default_group_body
                })
            end
          end
        end # context with bogus client

        context "with bogus user" do
          let(:new_group_payload) {{
              "groupname" => test_group,
              "actors" => {"clients" => [platform.non_admin_client.name],
                "users" => ["bogus"],
                "groups" => ["users"]}
            }}

          it "returns 400", :validation do
            skip "returns 500 instead" do
              put(request_url, platform.admin_user,
                :payload => new_group_payload).should look_like({
                  :status => 400
                })
              get(request_url, platform.admin_user).should look_like({
                  :status => 200,
                  :body_exact => default_group_body
                })
            end
          end
        end # context with bogus user

        context "with bogus group" do
          let(:new_group_payload) {{
              "groupname" => test_group,
              "actors" => {"clients" => [platform.non_admin_client.name],
                "users" => [platform.non_admin_user.name],
                "groups" => ["bogus"]}
            }}

          it "returns 400", :validation do
            skip "returns 500 instead" do
              put(request_url, platform.admin_user,
                :payload => new_group_payload).should look_like({
                  :status => 400
                })
              get(request_url, platform.admin_user).should look_like({
                  :status => 200,
                  :body_exact => default_group_body
                })
            end
          end
        end # context with bogus group

        context "with empty actors" do
          let(:new_group_payload) {{
              "groupname" => test_group,
              "actors" => {}
            }}

          it "will not update group (nothing changed)" do
            put(request_url, platform.admin_user,
              :payload => new_group_payload).should look_like({
                :status => 200
              })
            get(request_url, platform.admin_user).should look_like({
                :status => 200,
                :body_exact => default_group_body
              })
          end
        end # context with empty actors

        context "with no actors" do
          let(:new_group_payload) {{
              "groupname" => test_group
            }}

          it "will not update group (nothing changed)" do
            put(request_url, platform.admin_user,
              :payload => new_group_payload).should look_like({
                :status => 200
              })
            get(request_url, platform.admin_user).should look_like({
                :status => 200,
                :body_exact => default_group_body
              })
          end
        end # context with no actors

        context "with bogus actors" do
          let(:new_group_payload) {{
              "groupname" => test_group,
              "actors" => "bogus"
            }}

          it "returns 400", :validation do
            skip "returns 200(!) instead (but it doesn't actually change anything)" do
              put(request_url, platform.admin_user,
                :payload => new_group_payload).should look_like({
                  :status => 400
                })
              get(request_url, platform.admin_user).should look_like({
                  :status => 200,
                  :body_exact => default_group_body
                })
            end
          end
        end # context with bogus actors
      end # context updating groups
    end # context PUT /groups/<name>

    context "POST /groups/<name>" do
      context "admin user" do
        it "returns 405" do
          post(request_url, platform.admin_user).should look_like({
              :status => 405
            })
        end
      end
    end # context POST /groups/<name>
  end # context /groups/<name> endpoint
end # describe opscode-account group endpoint
