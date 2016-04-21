# -*- coding: utf-8 -*-
#
# Author:: Ho-Sheng Hsiao (<hosh@chef.io>)
# Copyright:: Copyright (c) 2013 Opscode, Inc.


describe "Org Creation", :org_creation do
  let(:requestor)  { superuser }
  let(:org)        { platform.test_org }
  let(:owner)      { platform.test_owner }

  let(:org_name)       { org.name }
  let(:owner_name)     { owner.name }
  let(:superuser_name) { superuser.name }
  let(:validator_name) { "#{org_name}-validator" }

  let(:request_method) { :GET }
  let(:expected_response) { ok_exact_response }
  let(:success_message)   { acl_body }

  let(:acl_body) do
    {
      "create" => {"actors" => create_actors, "groups" => create_groups},
      "read"   => {"actors" => read_actors,   "groups" => read_groups},
      "update" => {"actors" => update_actors, "groups" => update_groups},
      "delete" => {"actors" => delete_actors, "groups" => delete_groups},
      "grant"  => {"actors" => grant_actors,  "groups" => grant_groups}
    }
  end

  # By default, the superuser "pivotal" is always in the actors list
  let(:create_actors) { [ superuser_name ] }
  let(:read_actors)   { [ superuser_name ] }
  let(:update_actors) { [ superuser_name ] }
  let(:delete_actors) { [ superuser_name ] }
  let(:grant_actors)  { [ superuser_name ] }

  # By default, only admins have full CRUDG privs
  let(:create_groups) { admin_groups }
  let(:read_groups)   { admin_groups }
  let(:update_groups) { admin_groups }
  let(:delete_groups) { admin_groups }
  let(:grant_groups)  { admin_groups }

  let(:admin_groups)     { %w(admins) }
  let(:user_groups)      { %w(admins users) }
  let(:requestor_groups) { %w(admins clients users) }

  context "when validating default acls" do

    def self.should_have_default_acls_for(endpoint, *tags, &additional_examples)
      _url = "#{endpoint}/_acl"
      context "for /ORG#{_url}", *tags do
        let(:request_url) { api_url(_url) }

        instance_eval(&additional_examples) if additional_examples
        should_respond_with 200, 'and have default acl settings'
      end
    end

    context '[account]' do
      # This is for /ORGNAME/organizations/_acl
      should_have_default_acls_for "/organizations" do
        let(:create_groups) { admin_groups }
        let(:read_groups)   { user_groups }
        let(:update_groups) { admin_groups }
        let(:delete_groups) { admin_groups }
        let(:grant_groups)  { admin_groups }
      end

      should_have_default_acls_for "/containers/containers" do
        let(:create_groups) { admin_groups }
        let(:read_groups)   { user_groups }
        let(:update_groups) { admin_groups }
        let(:delete_groups) { admin_groups }
        let(:grant_groups)  { admin_groups }
      end

      should_have_default_acls_for "/containers/groups" do
        let(:create_groups) { admin_groups }
        let(:read_groups)   { user_groups }
        let(:update_groups) { admin_groups }
        let(:delete_groups) { admin_groups }
        let(:grant_groups)  { admin_groups }
      end
    end

    context '[chef]' do
      should_have_default_acls_for "/containers/clients" do
        let(:create_groups) { admin_groups }
        let(:read_groups)   { user_groups }
        let(:update_groups) { admin_groups }
        let(:delete_groups) { user_groups }
        let(:grant_groups)  { admin_groups }
      end

      should_have_default_acls_for "/containers/data" do
        let(:create_groups) { user_groups }
        let(:read_groups)   { requestor_groups }
        let(:update_groups) { user_groups }
        let(:delete_groups) { user_groups }
        let(:grant_groups)  { admin_groups }
      end

      should_have_default_acls_for "/containers/nodes" do
        let(:create_groups) { requestor_groups }
        let(:read_groups)   { requestor_groups }
        let(:update_groups) { user_groups }
        let(:delete_groups) { user_groups }
        let(:grant_groups)  { admin_groups }
      end

      should_have_default_acls_for "/containers/roles" do
        let(:create_groups) { user_groups }
        let(:read_groups)   { requestor_groups }
        let(:update_groups) { user_groups }
        let(:delete_groups) { user_groups }
        let(:grant_groups)  { admin_groups }
      end

      should_have_default_acls_for "/containers/environments" do
        let(:create_groups) { user_groups }
        let(:read_groups)   { requestor_groups }
        let(:update_groups) { user_groups }
        let(:delete_groups) { user_groups }
        let(:grant_groups)  { admin_groups }
      end

      should_have_default_acls_for "/containers/cookbooks" do
        let(:create_groups) { user_groups }
        let(:read_groups)   { requestor_groups }
        let(:update_groups) { user_groups }
        let(:delete_groups) { user_groups }
        let(:grant_groups)  { admin_groups }
      end
    end

    context '[default clients]' do
      should_have_default_acls_for "/clients/#{Pedant::Config.pedant_platform.test_org.name}-validator" do
        let(:create_groups) { admin_groups }
        let(:read_groups)   { user_groups }
        let(:update_groups) { admin_groups }
        let(:delete_groups) { user_groups }  # Users can't update a validator, but they can delete one
        let(:grant_groups)  { admin_groups }
      end
    end

    context '[default groups]' do
      should_have_default_acls_for "/groups/billing-admins" do
        # Not even admins have create, delete, or grant privs for billing-admins group
        let(:create_groups) { [] }
        let(:read_groups)   { %w(billing-admins) }
        let(:update_groups) { %w(billing-admins) }
        let(:delete_groups) { [] }
        let(:grant_groups)  { [] }
      end

      # These groups all have default privs, (only pivotal and admins)
      should_have_default_acls_for "/groups/admins"
      should_have_default_acls_for "/groups/clients"
      should_have_default_acls_for "/groups/users"
    end
  end
end
