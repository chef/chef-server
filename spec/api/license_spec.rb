# -*- coding: utf-8 -*-
#
# Author:: Douglas Triggs (<doug@getchef.com>)
# Copyright:: Copyright (c) 2014 Chef, Inc.

require 'pedant/rspec/node_util'

describe 'license' do
  include Pedant::RSpec::NodeUtil

  let (:request_url) { "#{platform.server}/license" }

  let (:node_count) { 0 }

  let (:response_body) {
    {
      "limit_exceeded" => (node_count > 25) ? true : false,
      "node_license" => 25,
      "node_count" => node_count,
      "upgrade_url" => /^http\:\/\.*/
    }}

  context "GET /license" do
    context "with no nodes" do

      it "returns 200 and correct body for superuser" do
        get(request_url, superuser).should look_like(
          :body_exact => response_body,
          :status => 200
          )
      end

      it "returns 200 and correct body for admin user" do
        get(request_url, platform.admin_user).should look_like(
          :body_exact => response_body,
          :status => 200
          )
      end

      it "returns 200 and correct body for normal user" do
        get(request_url, platform.non_admin_user).should look_like(
          :body_exact => response_body,
          :status => 200
          )
      end

      it "returns 401 for invalid user" do
        get(request_url, invalid_user).should look_like(
          :status => 401
          )
      end

      it "returns 401 for client" do
        get(request_url, platform.non_admin_client).should look_like(
          :status => 401
          )
      end

    end # with no nodes

    context "with nodes" do
      let(:nodes) do
        (1..node_count).map{|i| new_node(unique_name("pedant_node_list_test_#{i}"))}
      end

      before :each do
        nodes.each do |n|
          add_node(platform.admin_user, n)
        end
      end

      after :each do
        nodes.each do |n|
          delete_node(platform.admin_user, n['name'])
        end
      end

      context "with one node" do
        let(:node_count) { 1 }

        it "should return correct body for license status" do
          get(request_url, superuser).should look_like(
            :body_exact => response_body,
            :status => 200
            )
        end
      end # with one node

      context "with twenty-five nodes (license not exceeded)" do
        let(:node_count) { 25 }

        it "should return correct body for license status" do
          get(request_url, superuser).should look_like(
            :body_exact => response_body,
            :status => 200
            )
        end
      end # with twenty nodes

      context "with twenty-six nodes (license exceeded)" do
        let(:node_count) { 26 }

        it "should return correct body for license status" do
          get(request_url, superuser).should look_like(
            :body_exact => response_body,
            :status => 200
            )
        end
      end # with thirty nodes
    end # with nodes
  end # GET /license
end # license
