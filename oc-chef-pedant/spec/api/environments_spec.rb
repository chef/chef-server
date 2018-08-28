# -*- coding: utf-8 -*-
#
# Author:: Christopher Maier (<cm@chef.io>)
# Author:: John Keiser (<jkeiser@chef.io>)
# Author:: Douglas Triggs (<doug@chef.io>)
# Author:: Ho-Sheng Hsiao (<hosh@chef.io>)
# Copyright:: Copyright 2012-2018 Chef Software, Inc.
#

require 'pedant/rspec/auth_headers_util'
require 'pedant/rspec/environment_util'
require 'pedant/rspec/search_util'
require 'pedant/rspec/role_util'

describe "Environments API Endpoint", :environments do
  include Pedant::RSpec::EnvironmentUtil
  include Pedant::RSpec::SearchUtil
  include Pedant::RSpec::AuthHeadersUtil

  # include roles for testing w/ environments roles method
  include Pedant::RSpec::RoleUtil

  # TODO: pull this up somewhere else, eventually
  let(:organizations_url){ "#{server}/organizations" }

  # Deal with subtly different error messages/codes in one place

  let(:outside_user_not_associated_msg) { ["'#{outside_user.name}' not associated with organization '#{org}'"] }

  let(:new_environment_name) { 'pedant_testing_environment' }
  let(:non_existent_environment_name) { 'pedant_dummy_environment' }

  describe 'access control' do
    context 'GET /environments' do
      it 'returns a 401 ("Unauthorized") for invalid user', :authentication do
        get(api_url("/environments"),
            invalid_user) do |response|
          response.
            should look_like({
                               :status => 401,
                               :body_exact => {
                                 "error" => failed_to_authenticate_as_invalid_msg
                               }
                             })
        end
      end

      it 'returns a 403 ("Forbidden") for outside user', :authorization do
        get(api_url("/environments"),
            outside_user) do |response|
          response.should look_like({
                                      :status => 403,
                                      :body_exact => {
                                        "error" => outside_user_not_associated_msg
                                      }
                                    })
        end
      end
    end

    context 'GET /environments/<name>' do
      before(:each) do
        post(api_url("/environments"), admin_user,
             :payload => full_environment(new_environment_name))
      end

      after(:each) do
        delete(api_url("/environments/#{new_environment_name}"), admin_user)
      end

      it 'returns a 200 ("OK") for admin' do
        get(api_url("/environments/#{new_environment_name}"),
            admin_user) do |response|
          response.should look_like({
                                      :status => 200,
                                      :body => full_environment(new_environment_name)
                                    })
        end
      end

      it 'returns a 200 ("OK") for normal user' do
        get(api_url("/environments/#{new_environment_name}"),
            normal_user) do |response|
          response.should look_like({
                                      :status => 200,
                                      :body => full_environment(new_environment_name)
                                    })
        end
      end

      it 'returns a 401 ("Unauthorized") for invalid user', :authentication do
        get(api_url("/environments/#{new_environment_name}"),
            invalid_user) do |response|
          response.should look_like({
                                      :status => 401,
                                      :body_exact => {
                                        "error" => failed_to_authenticate_as_invalid_msg
                                      }
                                    })
        end
      end

      it 'returns a 403 ("Forbidden") for outside user', :authorization do
        get(api_url("/environments/#{new_environment_name}"),
            outside_user) do |response|
          response.should look_like({
                                      :status => 403,
                                      :body_exact => {
                                        "error" => outside_user_not_associated_msg
                                      }
                                    })
        end
      end

      it 'returns a 404 ("Not Found") for missing environment for admin' do

        get(api_url("/environments/#{non_existent_environment_name}"),
            admin_user) do |response|
          response.should look_like({
                                      :status => 404,
                                      :body_exact => {
                                        "error" => cannot_load_nonexistent_env_msg
                                      }
                                    })
        end
      end

      it 'returns a 404 ("Not Found") for missing environment for normal user' do

        get(api_url("/environments/#{non_existent_environment_name}"),
            normal_user) do |response|
          response.should look_like({
                                      :status => 404,
                                      :body_exact => {
                                        "error" => cannot_load_nonexistent_env_msg
                                      }
                                    })
        end
      end
    end

    context 'POST /environments' do
      after(:each) do
        delete(api_url("/environments/#{new_environment_name}"), admin_user)
      end

      it 'returns a 401 ("Unauthorized") for invalid user', :authentication do
        post(api_url("/environments"), invalid_user,
             :payload => new_environment(new_environment_name)) do |response|
          response.should look_like({
                                      :status => 401,
                                      :body_exact => {
                                        "error" => failed_to_authenticate_as_invalid_msg
                                      }
                                    })
        end

        # Verify that environment is not created

        get(api_url("/environments/#{new_environment_name}"),
            normal_user) do |response|
          response.should look_like({
                                      :status => 404,
                                      :body_exact => {
                                        "error" => cannot_load_new_env_msg
                                      }
                                    })
        end
      end

      it 'returns a 201 ("Created") for normal user' do
        post(api_url("/environments"), normal_user,
             :payload => new_environment(new_environment_name)) do |response|
          response.should look_like({
                                      :status => 201,
                                      :body => {
                                        "uri" => api_url("/environments/#{new_environment_name}")
                                      }
                                    })
        end
      end

      it 'returns a 403 ("Forbidden") for outside user', :authorization do
        post(api_url("/environments"), outside_user,
             :payload => new_environment(new_environment_name)) do |response|
          response.should look_like({
                                      :status => 403,
                                      :body_exact => {
                                        "error" => outside_user_not_associated_msg
                                      }
                                    })
        end

        # Verify that environment is not created

        get(api_url("/environments/#{new_environment_name}"),
            normal_user) do |response|
          response.should look_like({
                                      :status => 404,
                                      :body_exact => {
                                        "error" => cannot_load_new_env_msg
                                      }
                                    })
        end
      end

      it 'returns a 201 ("Created") for admin user' do
        post(api_url("/environments"),
             admin_user,
             :payload => new_environment(new_environment_name)) do |response|
          response.should look_like({
                                      :status => 201,
                                      :body => {
                                        "uri" => api_url("/environments/#{new_environment_name}")
                                      }
                                    })
        end
      end

      it 'returns a 409 ("Conflict") when trying to recreate existing environment' do
        environment_to_duplicate = new_environment(new_environment_name)
        add_environment(admin_user, environment_to_duplicate)

        post(api_url("/environments"),
             admin_user,
             :payload => environment_to_duplicate) do |response|
          response.should look_like({
                                      :status => 409,
                                      :body_exact => {
                                        "error" => ["Environment already exists"]
                                      }
                                    })
        end
      end
    end

    context 'PUT /environments/<name>' do
      before(:each) do
        post(api_url("/environments"), admin_user,
             :payload => full_environment(new_environment_name))
      end

      after(:each) do
        delete(api_url("/environments/#{new_environment_name}"), admin_user)
      end

      it 'returns a 200 ("OK") for admin' do
        put(api_url("/environments/#{new_environment_name}"),
            admin_user, :payload => full_environment(new_environment_name)) do |response|
          response.should look_like({
                                      :status => 200,
                                      :body => full_environment(new_environment_name)
                                    })
        end
      end

      it 'returns a 401 ("Unauthorized") for invalid user', :authentication do
        update = full_environment(new_environment_name)
        update['description'] = 'something different'

        put(api_url("/environments/#{new_environment_name}"), invalid_user,
            :payload => update) do |response|
          response.should look_like({
                                      :status => 401,
                                      :body_exact => {
                                        "error" => failed_to_authenticate_as_invalid_msg
                                      }
                                    })
        end

        # Verify that environment is not modified

        get(api_url("/environments/#{new_environment_name}"),
            admin_user) do |response|
          response.
            should look_like({
                               :status => 200,
                               :body_exact => full_environment(new_environment_name)
                             })
        end
      end

      it 'returns a 200 ("OK") for normal user' do
        put(api_url("/environments/#{new_environment_name}"),
            normal_user, :payload => full_environment(new_environment_name)) do |response|
          response.should look_like({
                                      :status => 200,
                                      :body => full_environment(new_environment_name)
                                    })
        end
      end

      it 'returns a 403 ("Forbidden") for outside user', :authorization do
        update = full_environment(new_environment_name)
        update['description'] = 'something different'

        put(api_url("/environments/#{new_environment_name}"), outside_user,
            :payload => update) do |response|
          response.should look_like({
                                      :status => 403,
                                      :body_exact => {
                                        "error" => outside_user_not_associated_msg
                                      }
                                    })
        end

        # Verify that environment is not modified

        get(api_url("/environments/#{new_environment_name}"),
            admin_user) do |response|
          response.
            should look_like({
                               :status => 200,
                               :body_exact => full_environment(new_environment_name)
                             })
        end
      end

      it 'returns a 404 ("Not Found") when PUTting missing environment' do
        put(api_url("/environments/#{non_existent_environment_name}"),
            admin_user, :payload => full_environment(new_environment_name)) do |response|
          response.should look_like({
                                      :status => 404,
                                      :body_exact => {
                                        "error" => cannot_load_nonexistent_env_msg
                                      }
                                    })
        end
      end
    end

    context 'DELETE /environments/<name>' do
      before(:each) do
        post(api_url("/environments"), admin_user,
             :payload => full_environment(new_environment_name))
      end

      after(:each) do
        delete(api_url("/environments/#{new_environment_name}"), admin_user)
      end

      it 'returns a 200 ("OK") as admin' do
        delete(api_url("/environments/#{new_environment_name}"),
               admin_user) do |response|
          response.should look_like({
                                      :status => 200,
                                      :body => full_environment(new_environment_name)
                                    })
        end
      end

      it 'returns a 200 ("OK") as user' do
        delete(api_url("/environments/#{new_environment_name}"),
               normal_user) do |response|
          response.should look_like({
                                      :status => 200,
                                      :body => full_environment(new_environment_name)
                                    })
        end
      end

      it 'returns a 401 ("Unauthorized") for invalid user', :authentication do
        delete(api_url("/environments/#{new_environment_name}"),
               invalid_user) do |response|
          response.should look_like({
                                      :status => 401,
                                      :body_exact => {
                                        "error" => failed_to_authenticate_as_invalid_msg
                                      }
                                    })
        end

        # Verify that environment is not deleted

        get(api_url("/environments/#{new_environment_name}"),
            admin_user) do |response|
          response.should look_like({
                                      :status => 200,
                                      :body => full_environment(new_environment_name)
                                    })
        end
      end

      it 'returns a 403 ("Forbidden") for outside user', :authorization do
        delete(api_url("/environments/#{new_environment_name}"),
               outside_user) do |response|
          response.should look_like({
                                      :status => 403,
                                      :body_exact => {
                                        "error" => outside_user_not_associated_msg
                                      }
                                    })
        end

        # Verify that environment is not deleted

        get(api_url("/environments/#{new_environment_name}"),
            admin_user) do |response|
          response.should look_like({
                                      :status => 200,
                                      :body => full_environment(new_environment_name)
                                    })
        end
      end

      it 'returns a 404 ("Not Found") when DELETEing missing environment' do

        delete(api_url("/environments/#{non_existent_environment_name}"),
               admin_user) do |response|
          response.should look_like({
                                      :status => 404,
                                      :body_exact => {
                                        "error" => cannot_load_nonexistent_env_msg
                                      }
                                    })
        end
      end
    end

  end
end
