# Copyright: Copyright (c) 2012 Opscode, Inc.
# License: Apache License, Version 2.0
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

module Pedant
  module OpenSource
    module ResponseBodies
      extend Pedant::Concern

      included do
        # Cross-endpoint Responses
        let(:unauthorized_access_credential_response) { open_source_bad_client_response }
        # FIXME: hard-coded the client name for now, not sure where to
        # get this from.
        let(:invalid_credential_error_message) { ["Failed to authenticate as 'invalid'. Ensure that your node_name and client key are correct."] }

        ["Failed to authenticate as 'invalid'. Ensure that your node_name and client key are correct."]

        # I suppose that, strictly speaking, these specific 'let's don't
        # have to be 'let's; i.e., they could be like this:
        #
        #   def open_source_bad_client_response
        #      {
        #        :status => 401,
        #        :body_exact => {
        #          "error" => ["Failed to authenticate. Ensure that your client key is valid."]
        #        }
        #    }
        #  end
        #
        #  Hosh: let() memoizes the value so it would be:
        #   def open_source_bad_client_response
        #     @_open_source_bad_client_response ||=  {
        #        :status => 401,
        #        :body_exact => {
        #          "error" => ["Failed to authenticate. Ensure that your client key is valid."]
        #        }
        #    }
        #  end
        #
        # Keeping them as 'let's is nice for the future if the responses
        # need to incorporate values of other 'let' blocks, though.

        # FIXME: hard-coded the client name for now, not sure where to
        # get this from.
        let(:open_source_bad_client_response) do
          http_401_response.with :body_exact,
            "error" => ["Failed to authenticate as 'bad_client'. Ensure that your node_name and client key are correct."]
        end

        let(:open_source_not_allowed_response) { forbidden_action_response }
        let(:forbidden_action_error_message) { ["You are not allowed to take this action."] }

        # Roles endpoint overrides

        ### A non-admin client cannot create a role for OSC, but a non-admin user can in OPC?!
        let(:create_role_as_non_admin_response) { open_source_not_allowed_response }

        ### OSC non-admin clients can't update a role?
        let(:update_role_as_non_admin_response) { open_source_not_allowed_response }

        ### OPC non-admin clients can't delete a role?
        ### Don't you mean OSC non-admin clients can't delete a role?
        let(:delete_role_as_non_admin_response) { open_source_not_allowed_response }

        # Cookbook endpoint overrides
        # Open-Source Chef Server uses these defaults values for cookbooks
        shared(:default_maintainer)       { 'YOUR_COMPANY_NAME' }
        shared(:default_maintainer_email) { 'YOUR_EMAIL' }
        shared(:default_license)          { "none" }

        let(:invalid_cookbook_version_error_message) { ["Invalid cookbook version '#{cookbook_version}'."] }

        # Sandbox Endpoints
        let(:sandbox_not_found_error_message) { ["Listing sandboxes not supported."] }

        # Impersonation Responses
        #
        # The 'failure_user' on Open Source is actually a client that
        # isn't valid, so it gets a 401
        let(:failure_user_impersonation_response) do
          unauthorized_response
        end
      end
    end
  end
end
