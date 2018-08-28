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


  context 'with non-default environments in the organization' do

    before(:each) { add_environment(admin_user, full_environment(new_environment_name)) }
    after(:each)  { delete_environment(admin_user, new_environment_name) }

    describe 'DELETE /environments/<name>' do
      let(:request_method) { :DELETE }
      let(:request_url)    { api_url "/environments/#{environment_name}" }

      let(:environment_name) { new_environment_name }

      context 'permissions' do
        let(:expected_response) { ok_exact_response }
        let(:success_message)   { full_environment(new_environment_name) }

        let(:deleted_environment_response) { get(request_url, admin_user) }

        def self.should_delete_named_environment_as(requestor_name, _requestor)
          context "as #{requestor_name}" do
            let(:requestor) { send _requestor }

            should_delete_named_environment
          end
        end

        def self.should_delete_named_environment
          should_respond_with 200, 'and the deleted environment'

          it 'should delete the environment' do
            response.should have_status_code 200
            deleted_environment_response.should look_like resource_not_found_response
          end
        end

        should_delete_named_environment_as 'an admin user', :admin_user
        should_delete_named_environment_as 'a normal user', :normal_user

        context 'with a user with minimal permissions to delete /environments/<name>' do
          before(:each) do
            restrict_permissions_to("/environments/#{new_environment_name}",
                                    normal_user => ['delete'])
          end
          let(:requestor) { normal_user }
          should_delete_named_environment
        end

        context 'with a user with all permissions EXCEPT delete' do
          let(:expected_response) { forbidden_response }
          let(:requestor) { normal_user }

          before(:each) do
            restrict_permissions_to("/environments/#{new_environment_name}",
                                    normal_user => %w(create read update grant))
          end

          should_respond_with 403
        end

        context 'with a client' do
          let(:expected_response) { forbidden_response }
          let(:requestor) { platform.non_admin_client }
          should_respond_with 403
        end

        context 'with an outside user (admin of another org)' do
          let(:expected_response) { forbidden_response }
          let(:requestor) { outside_user }
          # TODO we really should make this person admin of another org, for maximum
          # effectiveness.

          should_respond_with 403
        end
      end

      context 'when the organization does not exist' do
        let(:request_url) { "#{organizations_url}/doesnotexistatall/environments/#{new_environment_name}" }
        let(:expected_response) { resource_not_found_response }
        should_respond_with 404
      end

    end # DELETE /environments/<name>
  end # with non default environments
end # Environments endpoint
