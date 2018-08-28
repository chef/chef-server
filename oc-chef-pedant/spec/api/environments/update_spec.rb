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

describe "Environments API Endpoint", :environments do
  include Pedant::RSpec::EnvironmentUtil
  include Pedant::RSpec::AuthHeadersUtil

  # TODO: pull this up somewhere else, eventually
  let(:organizations_url){ "#{server}/organizations" }

  # Deal with subtly different error messages/codes in one place
  let(:outside_user_not_associated_msg) { ["'#{outside_user.name}' not associated with organization '#{org}'"] }
  let(:new_environment_name) { 'pedant_testing_environment' }
  let(:non_existent_environment_name) { 'pedant_dummy_environment' }

  let(:requestor) { admin_user }

  context 'with no additional environments' do

    before(:each) { delete_environment(admin_user, new_environment_name) }
    after(:each) { delete_environment(admin_user, new_environment_name) }


    context "PUT /environments/<name>" do
      let(:request_method)  { :PUT }
      let(:request_url)     { api_url "/environments/#{environment_name}" }
      let(:request_payload) { full_environment(environment_name) }

      context 'with a non-existent organization' do
        let(:request_url) { "#{organizations_url}/doesnotexistatall/environments/#{environment_name}" }
        let(:environment_name) { non_existent_environment_name }
        let(:expected_response) { resource_not_found_response }

        should_respond_with 404
      end
    end
  end

  context 'with non-default environments in the organization' do

    before(:each) { add_environment(admin_user, full_environment(new_environment_name)) }
    after(:each)  { delete_environment(admin_user, new_environment_name) }

    describe 'PUT /environments/<name>' do
      let(:request_method) { :PUT }
      let(:request_url)    { api_url "/environments/#{environment_name}" }
      let(:request_payload) { new_environment(environment_name) }
      let(:environment_name) { new_environment_name }

      # TODO: Use OPC ACL tests
      context 'permissions' do
        let(:modified_env) do
          full_environment(new_environment_name).merge({ 'description' => 'blah' })
        end

        shared_context 'successfully PUTs' do
          it "updates description = blah" do
            response = put(api_url("/environments/#{new_environment_name}"), requestor,
                           :payload => modified_env)
            response.should look_like({
                                        :status => 200,
                                        :body_exact => modified_env
                                      })
            get(api_url("/environments/#{new_environment_name}"), requestor).
              should look_like({
                                 :status => 200,
                                 :body_exact => modified_env
                               })
          end
        end
        context 'with an admin user' do
          let(:requestor) { admin_user }
          include_context 'successfully PUTs'
        end
        context 'with a normal user' do
          let(:requestor) { normal_user }
          include_context 'successfully PUTs'
        end
        context 'with a user with minimal permissions to update an environment' do
          it "updates description = blah" do
            restrict_permissions_to("/environments/#{new_environment_name}",
                                    normal_user => ['update'])
            response = put(api_url("/environments/#{new_environment_name}"), normal_user,
                           :payload => modified_env)
            response.should look_like({
                                        :status => 200,
                                        :body_exact => modified_env
                                      })
            unrestrict_permissions
            response = get(api_url("/environments/#{new_environment_name}"), normal_user)
            response.should look_like({
                                        :status => 200,
                                        :body_exact => modified_env
                                      })
          end
        end
        context 'with a user with all permissions EXCEPT update' do
          it 'returns 403', :authorization do
            restrict_permissions_to("/environments/#{new_environment_name}",
                                    normal_user => %w(create read delete grant))
            response = put(api_url("/environments/#{new_environment_name}"), normal_user,
                           :payload => modified_env)
            response.should look_like({:status => 403})
          end
        end
        context 'with a client' do
          it 'returns 403', :authorization do
            response = put(api_url("/environments/#{new_environment_name}"),
                           platform.non_admin_client,
                           :payload => modified_env)
            response.should look_like({:status => 403})
          end
        end
        context 'with an outside user (admin of another org)' do
          # TODO we really should make this person admin of another org, for maximum
          # effectiveness.
          it 'returns 403', :authorization do
            response = put(api_url("/environments/#{new_environment_name}"), outside_user,
                           :payload => modified_env)
            response.should look_like({:status => 403})
          end
        end
      end
    end
  end
end
