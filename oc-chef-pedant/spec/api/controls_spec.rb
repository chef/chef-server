#
# -*- indent-level => 4;indent-tabs-mode: nil; fill-column: 92 -*-
# ex => ts=4 sw=4 et
#
# Author => : Prajakta Purohit (<prajakta@chef.io>)
# Copyright => : Copyright (c) 2014 Opscode, Inc.
#

require 'pedant/rspec/common'
require 'pedant/rspec/node_util'

describe "Controls API Endpoint", :controls do
  it 'a POST to controls endpoint returns 410' do
    post(api_url("/controls"), platform.superuser, :payload => {} ) do |response|
      response.should look_like({
                                  :status => 410, :body_exact => {"error" => /\A/}
                                })
    end
  end

  it 'a GET to controls endpoint returns 410' do
    get(api_url("/controls"), platform.superuser) do |response|
      response.should look_like({
                                  :status => 410, :body_exact => {"error" => /\A/}
                                })
    end
  end
end
