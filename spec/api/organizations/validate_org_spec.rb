# -*- coding: utf-8 -*-
#
# Author:: Ho-Sheng Hsiao (<hosh@opscode.com>)
# Copyright:: Copyright (c) 2013 Opscode, Inc.


describe "Org Creation", :org_creation do
  let(:requestor)  { superuser }
  let(:org)        { platform.create_org(org_name) }
  let(:owner)      { platform.create_user(owner_name) }

  let(:org_name)   { unique_name("org_creation") }
  let(:owner_name) { unique_name("org_owner") }

  let(:org_with_owner) { platform.make_owner(owner, org) }

  before(:all) { org_with_owner }
  after(:all)  do
    platform.delete_org(org_name)
    platform.delete_user(owner)
  end

  context "when validating default containers" do
    let(:request_method) { :GET }
    let(:request_url)    { platform.api_url("/containers", org) }

    let(:default_containers) { %w(clients containers cookbooks data environments groups nodes roles sandboxes) }
    let(:default_container_hash) { Hash[*default_containers.map(&container_to_url).flatten]  }
    let(:container_to_url) { ->(x) { [x, platform.api_url("/containers/#{x}", org)] } }

    it 'should have default containers' do
      parsed_response.should eql(default_container_hash)
    end
  end

end
