# -*- coding: utf-8 -*-
#
# Author:: Tyler Cloke (<tyler@chef.io>)
# Copyright:: Copyright (c) 2014 Chef, Inc.

# TODO: there is no LDAP testing here because to
# accurately simulate the real use case, we would need
# the chef-server pedant is running on to be LDAP configured,
# and we would want that LDAP server to be down.
#
# This is too intergration-test-ish for pedant, but it is a case
# we should be testing. It'd be great if our CI test spun up an
# AD server, then configured and ran these tests with an LDAP user.
#
# LDAP status is not expected to affect this endpoint in any way
# but we would obviously want to test the server in a state we
# actually expect to see when this endpoint would be used in the real world.
#
# Basically what I'm saying is we should have integration testing and
# that pedant is the wrong place to do such an infrastructure / service-interactive test.
describe 'system_recovery', :users do

  let(:external_auth_id) {
    "#{Time.now.to_i}-#{Process.pid}"  }

  let(:username) {
    "recoverable_user-#{external_auth_id}"
  }

  let(:password) {
    "foobar"
  }

  let(:create_body) do
    {
      display_name: username,
      email: "#{username}@chef.io",
      password: password,
      username: username,
      external_auth_id: external_auth_id,
      recovery_authentication_enabled: true
    }
  end

  let(:user_body) {
    {
      'username' => username,
      'password' => password
    }
  }

  let(:request_url) { "#{platform.server}/system_recovery" }

  # create a new recovery_authentication_enabled:true user
  before :each do
    post("#{platform.server}/users", superuser, :payload => create_body)
  end

  # delete the user after test
  after :each do
    delete("#{platform.server}/users/#{username}", superuser)
  end

  describe "POST /system_recovery" do
    context "when a user has recovery_authentication_enabled == true is requested" do
      context "when the superuser is the requestor" do

        it "should return the user body" do
          post(request_url, superuser, :payload => user_body).should look_like(
            :body => {
              "display_name" => username,
              "username" => username,
              "email" => "#{username}@chef.io",
              "recovery_authentication_enabled" => true
            },
            :status => 200
          )
        end # should return the user body
      end # when the superuser is the requestor

      context "when the password passed is incorrect" do
        let(:wrong_pw_user_body) {
          { 'username' => username,
            'password' => "wrong_password"
          }
        }

        let(:error_message) {
          ["Failed to authenticate: Username and password incorrect"]
        }

        it "should return 401 with an error message" do
          post(request_url, superuser, :payload => wrong_pw_user_body).should look_like(
            :body => { "error" => error_message },
            :status => 401
          )
        end # should return 403 with an error message
      end # when the pasword passed is incorrect

      context "when a non-superuser is the requestor" do

        let(:error_message) {
          ["missing create permission"]
        }

        # TODO: the error string from opscode-account returns
        # the user in the body and not the requestor user
        it "should return 403 with an error explaining non-superuser is not authorized", :authorization do
          post(request_url, platform.admin_user, :payload => user_body).should look_like(
            :body => { "error" => error_message },
            :status => 403
          )
        end # should return 403 with an error explaining non-superuser is not authorized
      end # when a non-superuser is the requestor
    end # when a user has recovery_authentication_enabled == true is requested


    context "when a user has recovery_authentication_enabled != true is requested by the superuser" do

      let(:unrecoverable_external_auth_id) { "#{Time.now.to_i}-#{Process.pid}" }

      let(:unrecoverable_username) { "unrecoverable_user-#{unrecoverable_external_auth_id}" }

      let(:unrecoverable_user_create_body) do
        {
          display_name: unrecoverable_username,
          email: "#{unrecoverable_username}@chef.io",
          password: "foobar",
          username: unrecoverable_username,
          external_auth_id: unrecoverable_external_auth_id,
          recovery_authentication_enabled: false
        }
      end

      let(:unrecoverable_user_body) {
        { 'username' => unrecoverable_username,
          'password' => "foobar"
        }
      }

      let(:error_message) { ["System recovery disabled for this user"] }

      # create a new recovery_authentication_enabled:false user
      before :each do
        post("#{platform.server}/users", superuser, :payload => unrecoverable_user_create_body)
      end

      # delete the user after test
      after :each do
        delete("#{platform.server}/users/#{unrecoverable_username}", superuser)
      end

      it "should return 403 with a relevant error message" do
        post(request_url, superuser, :payload => unrecoverable_user_body).should look_like(
          # TODO: this error isn't quite terrible enough to mark test as pending,
          # but it should be "Requestor" not "User"
          :body => {
            "error" => error_message
          },
          :status => 403
        )
      end # should return 403 with a relevant error message
    end # when a user has recovery_authentication_enabled != true is requested by the superuser

    context "when a user that does not exist is requested by the superuser" do

      let(:user_body) {
        {
          'username' => 'this_user_does_not_exist-#{Time.now.to_i}-#{Process.pid}',
          'password' => password
        }
      }

      let(:error_message) { ["System recovery disabled for this user"] }

      it "should return 404 with an error message" do
        post(request_url, superuser, :payload => user_body).should look_like(
          :body => {
            "error" => error_message
          },
          :status => 403
        )
      end # should return 404 with an error message
    end # when a user that does not exist is requested by the superuser

    context "when the request is missing the username field" do

      let(:missing_username_body) { { 'password' => "foobar" } }
      let(:error_body) {{ "error" => ["Field 'username' missing"] }}

      it "should return 400 with an error message", :validation do
        post(request_url, superuser, :payload => missing_username_body).should look_like(
          :body => error_body,
          :status => 400
        )
      end # should return 400 with an error message
    end # when the request is missing the username field

    context "when the request is missing the password field" do

      let(:missing_username_body) {
        { "username" => username }
      }

      let(:error_body) {{ "error" => ["Field 'password' missing"] }}

      it "should return 400 with an error message", :validation do
        post(request_url, superuser, :payload => missing_username_body).should look_like(
          :body => error_body,
          :status => 400
        )
      end # should return 400 with an error message
    end # when the request is missing the password field

  end # POST /system_recovery
end # system_recovery
