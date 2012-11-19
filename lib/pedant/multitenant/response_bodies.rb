require 'pedant/multitenant/acl'

module Pedant
  module MultiTenant
    module ResponseBodies
      extend Pedant::Concern

      included do
        include Pedant::MultiTenant::ACL

        # Cross-endpoint Responses
        let(:unauthorized_access_credential_response) { multi_tenant_user_not_associated_response }
        let(:invalid_credential_error_message) do
          if ruby?
            ["Failed to authenticate as invalid. Ensure that your node_name and client key are correct."]
          else
            ["Failed to authenticate as 'invalid'. Ensure that your node_name and client key are correct."]
          end
        end

        let(:forbidden_action_error_message) { ruby? ? ["Merb::ControllerExceptions::Forbidden"] : ["missing delete permission"] }

        let(:multi_tenant_user_not_associated_response) do
          {
            status: 403,
            body_exact: { "error" => ["'#{outside_user.name}' not associated with organization '#{org}'"] }
          }
        end

        # Roles endpoint overrides

        ### A non-admin client cannot create a role for OSC, but a non-admin user can in OPC?!
        let(:create_role_as_non_admin_response) { create_role_success_response }

        ### OSC non-admin clients can't update a role?
        let(:update_role_as_non_admin_response) { update_role_success_response }

        ### OPC non-admin clients can't delete a role?
        ### Don't you mean OSC non-admin clients can't delete a role?
        let(:delete_role_as_non_admin_response) { delete_role_success_response }

        let(:invalid_role_response) { erlang_invalid_role_response }

        # Cookbook endpoint overrides
        let(:named_cookbook_org_path) { "/organizations/#{org}/cookbooks/#{cookbook_name}/#{cookbook_version}" }
        let(:invalid_cookbook_version_error_message) do
          if ruby?
            ["No routes match the request: #{named_cookbook_org_path}"]
          else
            ["Invalid cookbook version '#{cookbook_version}'."]
          end
        end

        let(:sandboxes_org_path) { "/organizations/#{org}/sandboxes" }
        let(:sandbox_not_found_error_message) { ["Listing sandboxes not supported."] }

        # Impersonation Responses
        #
        # The 'failure_user' on Private Chef is one not associated
        # with the organization, so it gets a 403
        let(:failure_user_impersonation_response) do
          forbidden_response
        end

      end # included
    end # ResponseBodies
  end # MultiTenant
end # Pedant
