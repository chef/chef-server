# -*- coding: utf-8 -*-
#
# Author:: Ho-Sheng Hsiao (<hosh@opscode.com>)
# Copyright:: Copyright (c) 2013 Opscode, Inc.


describe "Org Creation", :org_creation do
  let(:requestor)  { superuser }
  let(:org)        { platform.test_org }
  let(:owner)      { platform.test_owner }

  let(:request_method) { :GET }

  context "when validating default containers" do
    let(:request_url)    { api_url("/containers") }

    let(:default_containers) { %w(clients containers cookbooks data environments groups nodes roles sandboxes) }
    let(:default_container_hash) { Hash[*default_containers.map(&container_to_url).flatten]  }
    let(:container_to_url) { ->(x) { [x, platform.api_url("/containers/#{x}", org)] } }

    it 'should have default containers' do
      parsed_response.should eql(default_container_hash)
    end

    context 'for clients container' do
      let(:request_url)    { api_url("/containers/clients") }

      it 'should have default settings' do
        puts parsed_response
      end
    end
  end

end
