#
# -*- indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
# ex: ts=4 sw=4 et
#
# Author:: Douglas Triggs (<cm@chef.io>)
# Copyright:: Copyright (c) 2012 Opscode, Inc.
#

require 'pedant/rspec/common'

describe "chef server authorization checks", :authorization do
  def self.authorization_tests(resource_type)
    context "without starting resource" do
      before :each do
        delete(api_url("/#{resource_type}/#{new_name}"), admin_user)
      end

      after :each do
        delete(api_url("/#{resource_type}/#{new_name}"), admin_user)
      end

      context "POST /#{resource_type}" do
        shared_context "successful POST auth for #{resource_type}" do
          before(:each) do
            @response = post(api_url("/#{resource_type}"),
                             user,
                             :payload => new_resource)
          end

          it "returns 201 and a correct path" do
            @response.
              should look_like({
                                 :status => 201,
                                 :body_exact => {
                                   "uri" =>
                                   api_url("/#{resource_type}/#{new_name}")
                                 }
                               })
          end

          it "creates the resource" do
            # Verify that it comes back properly
            get(api_url("/#{resource_type}/#{new_name}"), user).
              should look_like({
                                 :status => 200,
                                 :body_exact => new_resource
                               })
          end
        end # shared_context "successful POST auth for #{resource_type}"

        context "with an admin user" do
          let(:user) { admin_user }
          it_behaves_like "successful POST auth for #{resource_type}"
        end # context "with an admin user"

        context "with a normal user" do
          let(:user) { normal_user }
          it_behaves_like "successful POST auth for #{resource_type}"
        end # context "with a normal user"

        context "with a user with minimal permissions to create resource" do
          before(:each) do
            restrict_permissions_to("/containers/#{resource_type}",
                                    normal_user => ["create"])
          end

          after(:each) do
            delete(api_url("/#{resource_type}/#{new_name}"), normal_user)
          end

          let(:user) { normal_user }

          it_behaves_like "successful POST auth for #{resource_type}" do
            it "creates a child with restricted permissions" do
              get(api_url("/#{resource_type}/#{new_name}/_acl"), superuser).
                should look_like({
                                   :status => 200,
                                   :body_exact => {
                                     "create" => { "actors" => [superuser.name,
                                                                normal_user.name],
                                       "groups" => []},
                                     "read" => { "actors" => [superuser.name,
                                                              normal_user.name],
                                       "groups" => []},
                                     "update" => { "actors" => [superuser.name,
                                                                normal_user.name],
                                       "groups" => []},
                                     "delete" => { "actors" => [superuser.name,
                                                                normal_user.name],
                                       "groups" => []},
                                     "grant" => { "actors" => [superuser.name,
                                                               normal_user.name],
                                       "groups" => []},
                                   }
                                 })
            end
          end # it_behaves_like "successful POST auth"
        end # context "with a user with minimal permissions to create resource"

        context "with a user with all permissions EXCEPT create" do
          it "returns 403", :authorization do
            restrict_permissions_to("/containers/#{resource_type}",
                                    normal_user => %w(read update delete grant))
            response = post(api_url("/#{resource_type}"),
                            normal_user,
                            :payload => new_resource)
            response.should look_like({:status => 403})
          end
        end # context "with a user with all permissions EXCEPT create"

        context "with a client", :pending do
          it "returns 403", :authorization do
            response = post(api_url("/#{resource_type}/"),
                            client,
                            :payload => new_resource)
            response.should look_like({:status => 403})
          end
        end # context "with a client"

        context "with an outside user (admin of another org)" do
          # TODO we really should make this person admin of another org, for maximum
          # effectiveness.
          it "returns 403", :authorization do
            response = post(api_url("/#{resource_type}/"),
                            outside_user,
                            :payload => new_resource)
            response.should look_like({:status => 403})
          end
        end # context "with an outside user (admin of another org)"
      end # context "POST /#{resource_type}"
    end # context "without starting resource"

    context "with starting resource" do
      before :each do
        post(api_url("/#{resource_type}"), admin_user, :payload => new_resource)
      end

      after :each do
        delete(api_url("/#{resource_type}/#{new_name}"), admin_user)
      end

      context "PUT /#{resource_type}/<name>" do
        shared_context "successful PUT auth for #{resource_type}" do
          it "updates resource" do
            response = put(api_url("/#{resource_type}/#{new_name}"), user,
                           :payload => modified_resource)
            response.should look_like({
                                        :status => 200,
                                        :body_exact => modified_resource
                                      })
            get(api_url("/#{resource_type}/#{new_name}"), user).
              should look_like({
                                 :status => 200,
                                 :body_exact => modified_resource
                               })
          end
        end # shared_context "successful PUT auth for #{resource_type}"

        context "with an admin user" do
          let(:user) { admin_user }
          include_context "successful PUT auth for #{resource_type}"
        end # context "with an admin user"

        context "with a normal user" do
          let(:user) { normal_user }
          include_context "successful PUT auth for #{resource_type}"
        end # context "with a normal user"

        context "with a user with minimal permissions to update a resource" do
          it "updates resource" do
            restrict_permissions_to("/#{resource_type}/#{new_name}",
                                    normal_user => ["update"])
            response = put(api_url("/#{resource_type}/#{new_name}"), normal_user,
                           :payload => modified_resource)
            response.should look_like({
                                        :status => 200,
                                        :body_exact => modified_resource
                                      })
            unrestrict_permissions
            response = get(api_url("/#{resource_type}/#{new_name}"), normal_user)
            response.should look_like({
                                        :status => 200,
                                        :body_exact => modified_resource
                                      })
          end
        end # context "with a user with minimal permissions to update a resource"

        context "with a user with all permissions EXCEPT update" do
          it "returns 403", :authorization do
            restrict_permissions_to("/#{resource_type}/#{new_name}",
                                    normal_user => %w(create read delete grant))
            response = put(api_url("/#{resource_type}/#{new_name}"), normal_user,
                           :payload => modified_resource)
            response.should look_like({:status => 403})
          end
        end # context "with a user with all permissions EXCEPT update"

        context "with a client", :pending do
          it "returns 403", :authorization do
            response = put(api_url("/#{resource_type}/#{new_name}"),
                           client,
                           :payload => modified_resource)
            response.should look_like({:status => 403})
          end
        end # context "with a client"

        context "with an outside user (admin of another org)" do
          # TODO we really should make this person admin of another org, for maximum
          # effectiveness.
          it "returns 403", :authorization do
            response = put(api_url("/#{resource_type}/#{new_name}"), outside_user,
                           :payload => modified_resource)
            response.should look_like({:status => 403})
          end
        end # context "with an outside user (admin of another org)"
      end # context "PUT /#{resource_type}/<name>"

      context "DELETE /#{resource_type}/<name>" do
        shared_context "successful DELETE auth for #{resource_type}" do
          it "DELETE /#{resource_type}/<name> succeeds" do
            response = delete(api_url("/#{resource_type}/#{new_name}"), user)
            response.should look_like({ :status => 200,
                                        :body => new_resource
                                      })
            response = get(api_url("/#{resource_type}/#{new_name}"), user)
            response.should look_like({ :status => 404 })
          end
        end # shared_context "successful DELETE auth for #{resource_type}"

        context "with an admin user" do
          let(:user) { admin_user }
          include_context "successful DELETE auth for #{resource_type}"
        end # context "with an admin user"

        context "with a normal user" do
          let(:user) { normal_user }
          include_context "successful DELETE auth for #{resource_type}"
        end # context "with a normal user"

        context "with a user with minimal permissions to delete a resource" do
          before(:each) do
            restrict_permissions_to("/#{resource_type}/#{new_name}",
                                    normal_user => ["delete"])
          end

          let(:user) { normal_user }

          include_context "successful DELETE auth for #{resource_type}"
        end # context "with a user with minimal permissions to delete a resource"

        context "with a user with all permissions EXCEPT delete" do
          it "returns 403", :authorization do
            restrict_permissions_to("/#{resource_type}/#{new_name}",
                                    normal_user => %w(create read update grant))
            response = delete(api_url("/#{resource_type}/#{new_name}"),
                              normal_user)
            response.should look_like({:status => 403})
          end
        end # context "with a user with all permissions EXCEPT delete"

        context "with a client", :pending do
          it "returns 403", :authorization do
            response = delete(api_url("/#{resource_type}/#{new_name}"),
                              client)
            response.should look_like({:status => 403})
          end
        end # context "with a client"

        context "with an outside user (admin of another org)" do
          # TODO we really should make this person admin of another org, for maximum
          # effectiveness.
          it "returns 403", :authorization do
            response = delete(api_url("/#{resource_type}/#{new_name}"),
                              outside_user)
            response.should look_like({:status => 403})
          end
        end # context "with an outside user (admin of another org)"
      end # context "DELETE /#{resource_type}/<name>"

      context "GET /#{resource_type}" do
        before(:each) do
          delete(api_url("/#{resource_type}/#{new_name}"), admin_user)
          delete(api_url("/#{resource_type}/#{other_name}"), admin_user)
          post(api_url("/#{resource_type}"), admin_user,
               :payload => new_resource) do |response|
            response.
              should look_like({
                                 :status => 201,
                                 :body_exact => {
                                   "uri" => api_url("/#{resource_type}/#{new_name}")
                                 }})
          end
          post(api_url("/#{resource_type}"), admin_user,
               :payload => modified_resource2) do |response|
            response.
              should look_like({
                                 :status => 201,
                                 :body_exact => {
                                   "uri" => api_url("/#{resource_type}/#{other_name}")
                                 }})
          end
        end

        after(:each) do
          delete(api_url("/#{resource_type}/#{other_name}"), admin_user)
        end

        shared_context "successful GET list auth for #{resource_type}" do
          it "GET /#{resource_type} succeeds" do
            response = get(api_url("/#{resource_type}"), user)
            response.
              should look_like({
                                 :status => 200,
                                 :body_exact => default_list
                               })
          end
        end # shared_context "successful GET list auth for #{resource_type}"

        context "with an admin user" do
          let(:user) { admin_user }
          include_context "successful GET list auth for #{resource_type}"
        end # context "with an admin user"

        context "with a normal user" do
          let(:user) { normal_user }
          include_context "successful GET list auth for #{resource_type}"
        end # context "with a normal user"

        context "with a user with minimal permissions to read resource" do
          before(:each) do
            restrict_permissions_to("/containers/#{resource_type}",
                                    normal_user => ["read"])
          end
          let(:user) { normal_user }
          include_context "successful GET list auth for #{resource_type}"
        end # context "with a user with minimal permissions to read resource"

        context "with a user with all permissions EXCEPT read" do
          it "returns 403", :authorization do
            restrict_permissions_to("/containers/#{resource_type}",
                                    normal_user => %w(create update delete grant))
            response = get(api_url("/#{resource_type}"), normal_user)
            response.should look_like({:status => 403})
          end
        end # context "with a user with all permissions EXCEPT read"

        context "with a client", :pending do
          let(:user) { client }
          include_context "successful GET list auth for #{resource_type}"
        end # context "with a client"

        context "with an outside user (admin of another org)" do
          # TODO we really should make this person admin of another org, for maximum
          # effectiveness.
          it "returns 403", :authorization do
            response = get(api_url("/#{resource_type}"), outside_user)
            response.should look_like({:status => 403})
          end
        end # context "with an outside user (admin of another org)"
      end # context "GET /#{resource_type}"

      context "GET /#{resource_type}/<name>" do
        shared_context "successful GET auth for #{resource_type}" do
          it "GET /#{resource_type}/<name> succeeds" do
            response = get(api_url("/#{resource_type}/#{new_name}"), user)
            response.
              should look_like({
                                 :status => 200,
                                 :body_exact => new_resource})
          end
        end # shared_context "successful GET auth for #{resource_type}"

        context "with an admin user" do
          let(:user) { admin_user }
          include_context "successful GET auth for #{resource_type}"
        end # context "with an admin user"

        context "with a normal user" do
          let(:user) { normal_user }
          include_context "successful GET auth for #{resource_type}"
        end # context "with a normal user"

        context "with a user with minimal permissions to read resource" do
          before(:each) do
            restrict_permissions_to("/#{resource_type}/#{new_name}",
                                    normal_user => ["read"])
          end
          let(:user) { normal_user }
          include_context "successful GET auth for #{resource_type}"
        end # context "with a user with minimal permissions to read resource"

        context "with a user with all permissions EXCEPT read" do
          it "returns 403", :authorization do
            restrict_permissions_to("/#{resource_type}/#{new_name}",
                                    normal_user => %w(create update delete grant))
            response = get(api_url("/#{resource_type}/#{new_name}"), normal_user)
            response.should look_like({:status => 403})
          end
        end # context "with a user with all permissions EXCEPT read"

        context "with a client", :pending do
          let(:user) { client }
          include_context "successful GET auth for #{resource_type}"
        end # context "with a client"

        context "with an outside user (admin of another org)" do
          # TODO we really should make this person admin of another org, for maximum
          # effectiveness.
          it "returns 403", :authorization do
            response = get(api_url("/#{resource_type}/#{new_name}"), outside_user)
            response.should look_like({:status => 403})
          end
        end # context "with an outside user (admin of another org)"
      end # context "GET /#{resource_type}/<name>"
    end # context "with starting resource"
  end # def authorization_tests

  # Generic setup
  let(:organizations_url){ "#{server}/organizations" }

  context "for roles" do
    # Endpoint specific setup -- for roles
    let(:resource_type) { "roles" }

    let(:new_name) { "pedant_testing_role" }
    let(:other_name) { "other_testing_role" }

    def new_role(name)
      {
        "name" => name,
        "description" => "blah",
        "json_class" => "Chef::Role",
        "default_attributes" => {},
        "override_attributes" => {},
        "chef_type" => "role",
        "run_list" => [],
        "env_run_lists" => {}
      }
    end

    let(:new_resource) { new_role(new_name) }
    let(:modified_resource) { new_resource.merge({ "description" => "foobar" }) }
    let(:modified_resource2) { new_resource.merge({ "name" => other_name }) }

    let(:default_list) { {
        new_name => api_url("/#{resource_type}/#{new_name}"),
        "#{other_name}" => api_url("/#{resource_type}/#{other_name}")
      } }

    authorization_tests("roles")
  end # context "for roles"

  context "for environments", :environments do
    # Endpoint specific setup -- for environment
    let(:resource_type) { "environments" }

    let(:new_name) { "pedant_testing_environment" }
    let(:other_name) { "other_testing_environment" }

    def new_environment(name)
      {
        "name" => name,
        "description" => "Behold, a testing environment!",
        "cookbook_versions" => {
          "ultimatecookbook" => "= 1.1.0"
        },
        "json_class" => "Chef::Environment",
        "chef_type" => "environment",
        "default_attributes" => {
          "defaultattr" => "y"
        },
        "override_attributes" => {
          "overrideattr" => "b"
        }
      }
    end

    let(:new_resource) { new_environment(new_name) }
    let(:modified_resource) { new_resource.merge({ "description" => "blah" }) }
    let(:modified_resource2) { new_resource.merge({ "name" => other_name }) }

    let(:default_list) { {
        "_default" => api_url("/#{resource_type}/_default"),
        new_name => api_url("/#{resource_type}/#{new_name}"),
        "#{other_name}" => api_url("/#{resource_type}/#{other_name}")
      } }

    authorization_tests("environments")
  end # context "for environments"

  context "for nodes", :nodes do
    # Endpoint specific setup -- for nodes
    let(:resource_type) { "nodes" }

    let(:new_name) { "pedant_testing_node" }
    let(:other_name) { "other_testing_node" }

    def new_node(name)
      {
      "name" => name,
      "json_class" => "Chef::Node",
      "chef_type" => "node",
      "chef_environment" => "_default",
      "override" => {},
      "normal" => {"is_anyone" => "no"},
      "default" => {},
      "automatic" => {},
      "run_list" => []
      }
    end

    let(:new_resource) { new_node(new_name) }
    let(:modified_resource) { new_resource.merge({ "run_list" => ["role[foobar]"] }) }
    let(:modified_resource2) { new_resource.merge({ "name" => other_name }) }

    let(:default_list) { {
        new_name => api_url("/#{resource_type}/#{new_name}"),
        "#{other_name}" => api_url("/#{resource_type}/#{other_name}")
      } }

    authorization_tests("nodes")
  end # context "for nodes"

  # TODO: Clients, sandboxes, data bags, and cookbooks all require
  # some sort of special Handling; users don't exist yet.
end # describe "private chef authorization checks"
