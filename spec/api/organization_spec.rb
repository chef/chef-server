# -*- coding: utf-8 -*-
#
# Author:: Ho-Sheng Hsiao (<hosh@opscode.com>)
# Copyright:: Copyright (c) 2013 Opscode, Inc.

describe "GET /organizations", :organizations do
  let(:request_method)    { :GET }
  let(:request_url)       { "#{platform.server}/organizations" }
  let(:requestor)         { superuser }

  let(:expected_response) { ok_response }

  should_respond_with 200
end
