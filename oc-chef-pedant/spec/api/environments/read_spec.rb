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
require 'pedant/rspec/search_util'

describe "Environments API Endpoint", :environments do
  include Pedant::RSpec::EnvironmentUtil
  include Pedant::RSpec::SearchUtil
  include Pedant::RSpec::AuthHeadersUtil

  # include roles for testing w/ environments roles method
  include Pedant::RSpec::RoleUtil

  # TODO: pull this up somewhere else, eventually
  let(:organizations_url){ "#{server}/organizations" }

  # Deal with subtly different error messages/codes in one place

  let(:outside_user_not_associated_msg) {
    ["'#{outside_user.name}' not associated with organization '#{org}'"] }

  let(:new_environment_name) { 'pedant_testing_environment' }
  let(:non_existent_environment_name) { 'pedant_dummy_environment' }

  let(:requestor) { admin_user }

  context 'GET' do
    let(:request_method) { :GET }

    context 'with no additional environments' do
      context 'GET /environments' do
        let(:request_url) { api_url "/environments" }
        let(:expected_response) { ok_exact_response }
        let(:success_message) { { "_default" => api_url("/environments/_default") } }

        should_respond_with 200
      end

      context 'GET /environments/_default' do
        let(:request_url) { api_url '/environments/_default' }
        let(:expected_response) { ok_exact_response }
        let(:success_message) do
          {
            "name" => "_default",
            "description" => "The default Chef environment",
            "cookbook_versions" => {},
            "json_class" => "Chef::Environment",
            "chef_type" => "environment",
            "default_attributes" => {},
            "override_attributes" => {}
          }
        end

        should_respond_with 200, 'and the default environment'
      end

      it 'should respond to cookbook versions'


      context 'with a non-existant environment' do
        let(:request_url) { api_url "/environments/#{non_existent_environment_name}" }
        let(:expected_response) { resource_not_found_exact_response }
        let(:not_found_error_message) { cannot_load_nonexistent_env_msg }

        should_respond_with 404
      end # with non-existant environment
    end # with no additional environments

    context 'with non-default environments' do
      before(:each) { add_environment(admin_user, full_environment(new_environment_name)) }
      after(:each)  { delete_environment(admin_user, new_environment_name) }


      context 'GET /environments' do
        include_context "environment_body_util"

        let(:other_env) {"other_testing_environment"}

        before(:each) do
          delete(api_url("/environments/#{new_environment_name}"), admin_user)
          delete(api_url("/environments/#{other_env}"), admin_user)
          post(api_url("/environments"), admin_user,
               :payload => default_payload) do |response|
            response.
              should look_like({
              :status => 201,
              :body_exact => {
              "uri" => api_url("/environments/#{new_environment_name}")
            }})
               end
          post(api_url("/environments"), admin_user,
               :payload => make_payload('name' => other_env)) do |response|
            response.
              should look_like({
              :status => 201,
              :body_exact => {
              "uri" => api_url("/environments/#{other_env}")
            }})
               end
        end

        after(:each) do
          delete(api_url("/environments/#{other_env}"), admin_user)
        end

        shared_context 'successfully GETs' do
          it 'GET /environments succeeds' do
            response = get(api_url("/environments"), user)
            response.
              should look_like({
              :status => 200,
              :body_exact => {
              "_default" => api_url("/environments/_default"),
              new_environment_name =>
            api_url("/environments/#{new_environment_name}"),
            "#{other_env}" =>api_url("/environments/#{other_env}")
            }})
          end
        end

        context 'permissions' do
          context 'with an admin user' do
            let(:user) { admin_user }
            include_context 'successfully GETs'
          end
          context 'with a normal user' do
            let(:user) { normal_user }
            include_context 'successfully GETs'
          end
          context 'with a user with minimal permissions to read /environments' do
            before(:each) do
              restrict_permissions_to('/containers/environments',
                                      normal_user => ['read'])
            end
            let(:user) { normal_user }
            include_context 'successfully GETs'
          end
          context 'with a user with all permissions EXCEPT read' do
            it 'returns 403', :authorization do
              restrict_permissions_to('/containers/environments',
                                      normal_user => %w(create update delete grant))
              response = get(api_url("/environments"), normal_user)
              response.should look_like({:status => 403})
            end
          end
          context 'with a client' do
            let(:user) { platform.non_admin_client }
            include_context 'successfully GETs'
          end
          context 'with an outside user (admin of another org)' do
            # TODO we really should make this person admin of another org, for maximum
            # effectiveness.
            it 'returns 403', :authorization do
              response = get(api_url("/environments"), outside_user)
              response.should look_like({:status => 403})
            end
          end
        end
      end

      context 'GET /environments/<name>' do
        let(:request_method) { :GET }
        let(:request_url)    { api_url "/environments/#{environment_name}" }

        let(:environment_name) { new_environment_name }


        shared_context 'successfully GETs environment' do
          let(:expected_response) { ok_exact_response }
          let(:success_message) { full_environment(new_environment_name) }

          should_respond_with 200
        end

        context 'permissions' do
          context 'with an admin user' do
            let(:requestor) { admin_user }
            include_context 'successfully GETs environment'
          end

          context 'with a normal user' do
            let(:requestor) { normal_user }
            include_context 'successfully GETs environment'
          end

          context 'with a user with minimal permissions to read /environments' do
            before(:each) do
              restrict_permissions_to("/environments/#{new_environment_name}",
                                      normal_user => ['read'])
            end
            let(:requestor) { normal_user }
            include_context 'successfully GETs environment'
          end

          context 'with a user with all permissions EXCEPT read' do
            it 'returns 403', :authorization do
              restrict_permissions_to("/environments/#{new_environment_name}",
                                      normal_user => %w(create update delete grant))
                                      response = get(api_url("/environments/#{new_environment_name}"), normal_user)
                                      response.should look_like({:status => 403})
            end

          end
          context 'with a client' do
            let(:requestor) { platform.non_admin_client }
            include_context 'successfully GETs environment'
          end

          context 'with an outside user (admin of another org)' do
            # TODO we really should make this person admin of another org, for maximum
            # effectiveness.
            let(:requestor) { outside_user }
            let(:expected_response) { forbidden_response }

            should_respond_with 403
          end
        end

        context 'when the organization does not exist' do
          let(:request_url) { "#{organizations_url}/doesnotexistatall/environments/#{new_environment_name}" }
          let(:expected_response) { resource_not_found_response }
          should_respond_with 404
        end

        context 'when the environment does not exist' do
          let(:environment_name) { 'doesnotexistatall' }
          let(:expected_response) { resource_not_found_response }
          should_respond_with 404
        end
      end

    end # with non-default environments

  end # GET
end # Environments API endpoint
