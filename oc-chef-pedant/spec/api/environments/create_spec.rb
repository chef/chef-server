# -*- coding: utf-8 -*-
#
# Author:: Christopher Maier (<cm@chef.io>)
# Author:: John Keiser (<jkeiser@chef.io>)
# Author:: Douglas Triggs (<doug@chef.io>)
# Author:: Ho-Sheng Hsiao (<hosh@chef.io>)
# Copyright:: Copyright (c) 2012 Opscode, Inc.
#

require 'pedant/rspec/auth_headers_util'
require 'pedant/rspec/environment_util'
#require 'pedant/rspec/role_util'

describe "Environments API Endpoint", :environments do
  include Pedant::RSpec::EnvironmentUtil
  include Pedant::RSpec::AuthHeadersUtil

  # include roles for testing w/ environments roles method
  #include Pedant::RSpec::RoleUtil

  let(:requestor) { admin_user }

  # TODO: pull this up somewhere else, eventually
  let(:organizations_url){ "#{server}/organizations" }

  # Deal with subtly different error messages/codes in one place

  let(:outside_user_not_associated_msg) {
    ["'#{outside_user.name}' not associated with organization '#{org}'"] }

    let(:new_environment_name) { 'pedant_testing_environment' }
    let(:non_existent_environment_name) { 'pedant_dummy_environment' }
    context 'POST /environments' do
      let(:request_method)  { :POST }
      let(:request_url)     { api_url '/environments' }
      let(:request_payload) { full_environment(new_environment_name) }

      let(:expected_response) { resource_created_exact_response }
      let(:created_resource)  { { "uri" => api_url("/environments/#{new_environment_name}") } }
      let(:persisted_resource) { get_environment requestor, new_environment_name }


      context 'with no additional environments' do
        before(:each) { delete_environment(admin_user, new_environment_name) }
        after(:each) { delete_environment(admin_user, new_environment_name) }


        def self.should_create_an_environment(&additional_examples)
          should_respond_with 201, 'and a correct path'

          it 'should persist the environment' do
            response.should have_status_code 201

            # Verify that it comes back properly
            persisted_resource.should look_like http_200_response.with(body_exact: full_environment(new_environment_name))
          end

          instance_eval(&additional_examples) if additional_examples
        end

        context 'permissions' do
          context 'with an admin user' do
            let(:requestor) { admin_user }
            should_create_an_environment
          end
          context 'with a normal user' do
            let(:requestor) { normal_user }
            should_create_an_environment
          end

          context 'with a user with minimal permissions to create /environments', :platform => :multitenant do
            before(:each) do
              restrict_permissions_to('/containers/environments',
                                      normal_user => ['create'])
            end
            after(:each) do
              delete(api_url("/environments/#{new_environment_name}"), normal_user)
            end

            let(:requestor) { normal_user }

            should_create_an_environment do
              let(:acl_response) do
                http_200_response.with body_exact: {
                  'create' => { 'actors' => [superuser.name, normal_user.name], 'groups' => []},
                  'read' => { 'actors' => [superuser.name, normal_user.name], 'groups' => []},
                  'update' => { 'actors' => [superuser.name, normal_user.name], 'groups' => []},
                  'delete' => { 'actors' => [superuser.name, normal_user.name], 'groups' => []},
                  'grant' => { 'actors' => [superuser.name, normal_user.name], 'groups' => []},
                }

              end

              it 'should create a child with restricted permissions' do
                response.should have_status_code 201
                get(api_url("/environments/#{new_environment_name}/_acl"), superuser).should look_like acl_response
              end
            end
          end

          context 'with a user with all permissions EXCEPT create' do
            let(:requestor)         { normal_user }
            let(:request_payload)   { new_environment(new_environment_name) }
            let(:expected_response) { forbidden_response }

            it 'returns 403', :authorization do
              restrict_permissions_to('/containers/environments',
                                      normal_user => %w(read update delete grant))
              response.should look_like expected_response
            end
          end

          context 'with a client' do
            let(:requestor)         { platform.non_admin_client }
            let(:request_payload)   { new_environment(new_environment_name) }
            let(:expected_response) { forbidden_response }

            should_respond_with 403
          end

          context 'with an outside user (admin of another org)' do
            let(:requestor)         { outside_user }
            let(:request_payload)   { new_environment(new_environment_name) }
            let(:expected_response) { forbidden_response }
            # TODO we really should make this person admin of another org, for maximum
            # effectiveness.
            # TODO --> Add that test to the new ACL test macros, "as outside admin"

            should_respond_with 403
          end
        end

        skip 'when handling the payload', :pedantic do
          context 'with authentication headers' do
            # Unconverted Auth Header DSL
            let(:method) { request_method }
            let(:url)    { request_url }
            let(:body)   { request_payload }
            let(:response_should_be_successful) do
              response.
                should look_like({
                :status => 201,
                :body_exact => {
                "uri" => api_url("/environments/#{new_environment_name}")
              }
              })
            end

            let(:success_user) { admin_user }
            let(:failure_user) { outside_user }

            include_context 'handles authentication headers correctly'
          end # with authentication headers

          context 'when unable to accept application/json' do
            let(:request_headers) { { "Accept" => "application/xml" } }
            let(:expected_response) { not_acceptable_response }

            should_respond_with 406
          end

          context 'when sending something other than application/json' do
            let(:request_headers) { { "Content-Type" => "application/xml" } }
            let(:request_payload) { '<environment name="blah"></environment>' }

            skip "webmachine not sucking" do
              should_respond_with 415
            end
          end

          context 'with unparsable JSON payload' do
            let(:request_payload) { '{"hi' }
            let(:expected_response) { bad_request_response }

            should_respond_with 400
          end

          context 'with empty request payload' do
            let(:request_payload) { '' }
            let(:expected_response) { bad_request_response }

            should_respond_with 400
          end

          context 'with oversized payload' do
            let(:request_payload) { '{"x" = [' + ("\"xxxxxxxx\",\n"*200000) + '"xxxxxxxx"]}' }

            let(:expected_response) { request_entity_too_large_response }
            should_respond_with 413
          end

          context 'with a non-existent organization' do
            let(:request_url) { "#{organizations_url}/doesnotexistatall/environments" }
            let(:expected_response) { resource_not_found_response }

            should_respond_with 404
          end
        end

      end # with no additional environments
    end # POST /environments
end # Environments Endpoint
