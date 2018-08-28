# -*- coding: utf-8 -*-
#
# Author:: Ho-Sheng Hsiao (<hosh@chef.io>)
# Copyright:: Copyright 2013-2018 Chef Software, Inc.


describe "Org Creation", :org_creation do
  let(:requestor)  { superuser }
  let(:org)        { platform.test_org }
  let(:owner)      { platform.test_org_owner }

  let(:org_name)       { org.name }
  let(:owner_name)     { owner.name }
  let(:superuser_name) { superuser.name }
  let(:validator_name) { "#{org_name}-validator" }

  let(:request_method) { :GET }

  context "when validating default groups" do
    let(:request_url)    { api_url("/groups") }

    let(:default_groups) { %w(admins billing-admins clients users) }
    let(:default_group_hash) { Hash[*default_groups.map(&group_to_url).flatten]  }
    let(:group_to_url) { ->(x) { [x, platform.api_url("/groups/#{x}", org)] } }

    it 'should have default groups' do
      expect(parsed_response).to loosely_match default_group_hash
    end

    def self.should_have_group_for(resource, &additional_examples)
      resource = resource.to_s
      context "for #{resource} group" do
        let(:request_url)       { api_url("/groups/#{resource}") }
        let(:expected_response) { ok_exact_response }
        let(:success_message) do
          { "actors"    => [superuser_name, owner_name],
            "users"     => [superuser_name, owner_name],
            "clients"   => [],
            "groups"    => [],
            "orgname"   => org_name,
            "name"      => resource,
            "groupname" => resource }
        end

        instance_eval(&additional_examples) if additional_examples

        should_respond_with 200, 'and have default settings'
      end
    end

    should_have_group_for :admins do
      let(:success_message) do
        { "actors"    => [superuser_name, owner_name],
          "users"     => [superuser_name, owner_name],
          "clients"   => [],
          "groups"    => [],
          "orgname"   => org_name,
          "name"      => 'admins',
          "groupname" => 'admins' }
      end
    end

    should_have_group_for 'billing-admins' do
      let(:success_message) do
        { "actors"    => [owner_name],
          "users"     => [owner_name],
          "clients"   => [],
          "groups"    => [],
          "orgname"   => org_name,
          "name"      => 'billing-admins',
          "groupname" => 'billing-admins' }
      end
    end

    should_have_group_for :users do
      let(:expected_response) { ok_full_response }
      let(:success_message) do
        { "actors"    => [superuser_name, owner_name],
          "users"     => [superuser_name, owner_name],
          "clients"   => [],
          "orgname"   => org_name,
          "name"      => 'users',
          "groupname" => 'users' }
      end

      let(:member_groups) { parsed_response['groups'] }

      # USAG implementation going forward is iffy. Rather than figure out
      # if the USAG in the users group is the one created for the owner,
      # we're just going to check for the presence of group names with
      # hexdecimals.
      it 'should have a USAG', :usags do
        expect(member_groups.select { |g| g =~ /^[0-9a-f]+$/ }).not_to be_empty
      end
    end

    should_have_group_for :clients do
      let(:success_message) do
        { "actors"    => [validator_name],
          "users"     => [],
          "clients"   => [validator_name],
          "groups"    => [],
          "orgname"   => org_name,
          "name"      => 'clients',
          "groupname" => 'clients' }
      end
    end
  end

end
