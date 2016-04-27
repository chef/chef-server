# -*- coding: utf-8 -*-
#
# Author:: Ho-Sheng Hsiao (<hosh@chef.io>)
# Copyright:: Copyright (c) 2013 Opscode, Inc.


describe "Org Creation", :org_creation do
  let(:requestor)  { platform.test_org_owner }
  let(:org)        { platform.test_org }

  let(:request_method) { :GET }

  context "when validating default containers" do
    let(:request_url)    { api_url("/containers") }

    let(:default_containers) { %w(clients containers cookbooks data environments groups nodes roles sandboxes policies policy_groups cookbook_artifacts) }
    let(:default_container_hash) { Hash[*default_containers.map(&container_to_url).flatten]  }
    let(:container_to_url) { ->(x) { [x, platform.api_url("/containers/#{x}", org)] } }

    it 'should have default containers' do
      # This also tests to make sure there are no extraneous containers in the response
      expect(parsed_response).to eql(default_container_hash)
    end

    def self.should_have_container_for(resource)
      # The exact message currently has "containerpath", but it is universally ignored
      # We don't want to validate it, since it might be taken out in the port.
      # If that happens, the expected_response should be ok_exact_response to keep
      # validations stricter.

      resource = resource.to_s

      context "for #{resource} container" do
        let(:request_url)       { api_url("/containers/#{resource}") }
        let(:expected_response) { ok_full_response }
        let(:success_message)   { {"containername" => resource } }


        should_respond_with 200, 'and have default settings'
      end
    end

    should_have_container_for :clients
    should_have_container_for :containers
    should_have_container_for :cookbooks
    should_have_container_for :data
    should_have_container_for :environments
    should_have_container_for :groups
    should_have_container_for :nodes
    should_have_container_for :roles
    should_have_container_for :sandboxes
    should_have_container_for :policies
    should_have_container_for :policy_groups
    should_have_container_for :cookbook_artifacts

  end

end
