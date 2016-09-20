# -*- coding: utf-8 -*-
#
# Author:: Ho-Sheng Hsiao (<hosh@chef.io>)
# Author:: Tyler Cloke (<tyler@chef.io>)
# Copyright:: Copyright (c) 2013 Opscode, Inc.

require 'securerandom'
require 'pedant/rspec/common'
require 'json'

describe "/organizations", :organizations do

  let(:org_with_no_name) { { 'full_name' => "Test This Org" } }
  let(:org_with_no_full_name) { { 'name' => orgname } }
  let(:org_with_bad_name ) { { 'name' => "@!## !@#($@" } }
  describe "GET /organizations" do
    let(:request_url)    { "#{platform.server}/organizations" }
    let(:requestor)      { superuser }

    context "when the user requests a list of organizations" do
      it "should return a valid list of organizations" do
        get(request_url, requestor).should look_like(
          # Would actually probably contain additional orgs in body, but this is the only
          # one we know is there.
          :body => {
            platform.test_org.name => "#{platform.server}/organizations/#{platform.test_org.name}"
          },
          :status => 200
        )
      end
    end

  end

  describe "GET /organizations/:id" do
    let(:request_url)    { "#{platform.server}/organizations/#{platform.test_org.name}" }
    let(:requestor)      { superuser }

    context "when the user requests a valid organization" do

      it "should return a valid organization object" do
        get(request_url, requestor).should look_like(
          # Would actually probably contain additional orgs in body, but this is the only
          # one we know is there.
          :body => {
            "name"                     => platform.test_org.name,
            "full_name"                => platform.test_org.name
            # -- TODO - remove these
            # I'm only commenting them out in case the webui depends on one of these
            # and we determine that we need to come back and re-implement them at some
            # point
            # "org_type"                 => "Business",
            # "clientname"               => "#{platform.test_org.name}-validator",
            # "chargify_subscription_id" => nil,
            # "chargify_customer_id"     => nil,
            # "billing_plan"             => "platform-free",
          },
          :status => 200
        )

      end

      it "should return a organization object that contains a valid guid" do
        parsed_response = JSON.parse(get(request_url, requestor))
        parsed_response["guid"].size.should == 32
      end
    end

  end

  describe "POST /organizations" do
    let(:orgname) { "test-#{Time.now.to_i}-#{SecureRandom.hex}-#{Process.pid}" }
    after :each do
      platform.delete_org(orgname)
    end

    let(:request_body) do
      {
        full_name: "fullname-#{orgname}",
        name: orgname,
        org_type: "Business",
      }
    end

    context "when the superuser posts a new organization with a valid body and name" do
      it "should respond with a valid newly created organization" do
        post("#{platform.server}/organizations", superuser, :payload => request_body).should look_like(
          :body => {
            "clientname" => "#{orgname}-validator",
            "uri" => "#{platform.server}/organizations/#{orgname}"
          },
          :status => 201
        )
      end
    end

    context "when a non-superuser posts a new organization with a valid body and name" do
      it "should respond with a valid newly created organization" do
        post("#{platform.server}/organizations", normal_user, :payload => request_body).should look_like(
          :status => 403
        )
      end
    end

    context "when a non-superuser in server-admins posts a new organization with a valid body and name" do
      it "should respond with a valid newly created organization" do
        require 'pp'; pp server_admin_user: platform.server_admin_user, platform_keys: platform.methods.sort - Object.methods

        post("#{platform.server}/organizations", platform.server_admin_user, :payload => request_body).should look_like(
          :body => {
            "clientname" => "#{orgname}-validator",
            "uri" => "#{platform.server}/organizations/#{orgname}"
          },
          :status => 201
        )
      end
    end

    context "but an organization of the same name already exists" do
      before :each do
        post("#{platform.server}/organizations", superuser, :payload => request_body)
      end

      it "it rejects the new org as conflicting" do
        post("#{platform.server}/organizations", superuser, :payload => request_body).should look_like( :status => 409)
      end
    end

      # Note:
      # Currently excluded because it fails intermittently.
      # To re-enable, please remove ', :intermittent_failure => true'
    it "should respond with data containing a valid private key",  :intermittent_failure => true do
      result = JSON.parse(post("#{platform.server}/organizations", superuser, :payload => request_body))
      /-----BEGIN RSA PRIVATE KEY-----/.should match(result["private_key"])
    end

    context "when the user attempts to create a new org with invalid data", :validation do
      it "it should fail when 'name' is missing" do
        post("#{platform.server}/organizations", superuser, :payload => org_with_no_name ).should look_like(
          :status => 400
        )
      end
      it "it should fail when 'full_name' is missing" do
        post("#{platform.server}/organizations", superuser, :payload => org_with_no_full_name ).should look_like(
          :status => 400
        )
      end
      it "it should fail when 'name' is invalid", :validation do
        post("#{platform.server}/organizations", superuser, :payload => org_with_bad_name).should look_like(
          :status => 400
        )
      end
    end

  end

  describe "PUT /organizations/:id" do
    let(:orgname) { "test-#{Time.now.to_i}-#{Process.pid}" }
    let(:post_request_body) do
      {
        full_name: "fullname-#{orgname}",
        name: orgname,
        org_type: "Business"
      }
    end

    before do
      post("#{platform.server}/organizations", superuser, :payload => post_request_body)
    end

    after do
      platform.delete_org(orgname)
    end

    context "when the user updates the organization object" do
      let(:new_orgname) { "update-to-#{Time.now.to_i*3}-#{Process.pid}" }
      let(:payload) do
        {
          'name' => new_orgname,
          'org_type' => "Pleasure",
          'full_name' => new_orgname
        }
      end

      # our standard response for erlang requests is to return the json that was posted
      let(:update_response_body) do
          payload
      end

      # since we no longer track or return 'org_type', ignore it in erlang mode
      let(:get_response_body) do
        resp = payload.dup
        resp.delete('org_type')
        resp
      end

      it "should fail to update the organization object if the name is changed" do
        put("#{platform.server}/organizations/#{orgname}", superuser, :payload => payload).should look_like(
          :status => 400,
          :body => { "error" => ["Field 'name' invalid"] }
        )
      end
    end

    context "when the user updates the organization object update should fail when", :validation do
      let(:orgname) { "test-#{Time.now.to_i}-#{Process.pid}" }
      let(:post_request_body) do
        {
          full_name: "fullname-#{orgname}",
          name: orgname,
          org_type: "Business"
        }
      end

      before do
        post("#{platform.server}/organizations", superuser, :payload => post_request_body)
      end

      after do
        platform.delete_org(orgname)
      end

      it "'name' is missing" do
        put("#{platform.server}/organizations/#{orgname}", superuser, :payload => org_with_no_name ).should look_like(
          :status => 400
        )
      end
      it "'full_name' is missing" do
        put("#{platform.server}/organizations/#{orgname}", superuser, :payload => org_with_no_full_name ).should look_like(
          :status => 400
        )
      end
      it "'name' is invalid" do
        put("#{platform.server}/organizations/#{orgname}", superuser, :payload => org_with_bad_name ).should look_like(
          :status => 400
        )
      end

    end

    context "when the user updates fields in the organization with valid data" do
      let(:payload) do
        {
          'name' => orgname,
          'org_type' => "Pleasure",
          'full_name' => "A Real Org Name"
        }
      end

      # our standard response for erlang requests is to return the json that was posted
      let(:update_response_body) { payload }

      # since we no longer track or return 'org_type', ignore it in erlang mode
      let(:get_response_body) do
        resp = payload.dup
        resp.delete('org_type')
        resp

      end

      it "should update the organization object" do
        # TODO: we don't validate org_type at all
        put("#{platform.server}/organizations/#{orgname}", superuser, :payload => payload).should look_like(
          :status => 200,
          :body => update_response_body
        )

        get("#{platform.server}/organizations/#{orgname}", superuser).should look_like(
          :status => 200,
          :body => get_response_body
        )
      end
    end

    context "when the user tries to PUT to the organization with a private_key", :validation do
      it "throws an error related to no longer supporting PUT for key updating" do
        request = put("#{platform.server}/organizations/#{orgname}", superuser,
                      :payload => {
                        'name' => orgname,
                        'private_key' => "some_unused_key"
                      })

        request.should look_like(
                                 :status => 400
                                 )

        JSON.parse(request).should have_key("error")
      end
    end

  end


end
