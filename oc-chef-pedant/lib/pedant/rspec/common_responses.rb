# Copyright: Copyright 2012-2018 Chef Software, Inc.
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

require 'pedant/concern'
require 'pedant/core_ext/hash'

module Pedant
  module RSpec
    module CommonResponses
      extend Pedant::Concern

      # Include this to use generic server responses

      included do
        # Named HTTP responses
        let(:ok_response)                       { http_200_response }
        let(:created_response)                  { http_201_response }
        let(:bad_request_response)              { http_400_response }
        let(:unauthorized_response)             { http_401_response }
        let(:forbidden_response)                { http_403_response }
        let(:not_found_response)                { http_404_response }
        let(:resource_not_found_response)       { http_404_response }
        let(:method_not_allowed_response)       { http_405_response }
        let(:not_acceptable_response)           { http_406_response }
        let(:conflict_response)                 { http_409_response }
        let(:request_entity_too_large_response) { http_413_response }
        let(:unsupported_media_type_response)   { http_415_response }
        let(:server_error_response)             { http_500_response }


        # TODO: Refactor tests to use these generic responses
        let(:ok_full_response)                  { http_200_full_response }
        let(:ok_exact_response)                 { http_200_exact_response }
        let(:created_exact_response)            { http_201_exact_response }
        let(:resource_created_full_response)    { http_201_full_response }
        let(:resource_created_exact_response)   { http_201_exact_response }
        let(:bad_request_exact_response)        { http_400_exact_response }
        let(:forbidden_exact_response)          { http_403_exact_response }
        let(:resource_not_found_exact_response) { http_404_exact_response }
        let(:method_not_allowed_exact_response) { http_405_exact_response }
        let(:conflict_exact_response)           { http_409_exact_response }

        # HTTP error code responses
        # TODO: There has got to be a better way to manage this
        let(:http_200_response)       { { status: 200 } }
        let(:http_200_full_response)  { http_200_response.with(:body, success_message) }
        let(:http_200_exact_response) { http_200_response.with(:body_exact, success_message) }

        let(:http_201_response)        { { status: 201 } }
        let(:http_201_full_response)   { http_201_response.with(:body, created_resource) }
        let(:http_201_exact_response)  { http_201_response.with(:body_exact, created_resource) }
        let(:created_resource)         { fail "Define created_resource this in your test" }

        let(:http_400_response)        { { status: 400 } }
        let(:http_400_exact_response)  { http_400_response.with(:body_exact, "error" => error_message) }

        let(:http_401_response) { { status: 401 } }

        let(:http_403_response)       { { status: 403 } }
        let(:http_403_exact_response) { http_403_response.with(:body_exact, "error" => error_message) }

        let(:http_404_response) { { status: 404 } }
        # TODO: Do we use generic error_message or make it semantically descriptive?
        let(:http_404_exact_response) { http_404_response.with(:body_exact, "error" => not_found_error_message) }

        let(:http_405_response) { { status: 405 } }
        let(:http_405_exact_response) { http_405_response.with(:body_exact, "error" => error_message) }

        let(:http_406_response) { { status: 406 } }

        let(:http_409_response)       { { status: 409 } }
        let(:http_409_exact_response) { http_409_response.with(:body_exact, "error" => conflict_error_message) }

        # TODO:Refactor this to use exact_ naming convention
        let(:http_413_response) { { status: 413 } }
        let(:http_413_full_response) { http_413_response.with(:body, "error" => "JSON must be no more than 1000000 bytes.") }

        let(:http_415_response) { { status: 415 } }
        let(:http_500_response) { { status: 500 } }

        let(:multi_tenant_user_not_associated_text) { "'#{outside_user.name}' not associated with organization '#{org}'" }
        let(:multi_tenant_user_not_associated_response) do
          {
            status: 403,
            body_exact: { "error" => [multi_tenant_user_not_associated_text] }
          }
        end
        # Cross-endpoint responses
        let(:unauthorized_access_credential_response) { multi_tenant_user_not_associated_response }
        let(:invalid_credential_error_message) { ["Failed to authenticate as 'invalid'. Ensure that your node_name and client key are correct."] }
        let(:invalid_credential_exact_response) { http_401_response.with(:body_exact, "error" => invalid_credential_error_message) }
        let(:invalid_access_credential_exact_response) { fail 'Override this in response_bodies' }

        let(:forbidden_action_response) { http_403_response.with(:body_exact, "error"=> forbidden_action_error_message) }
        let(:forbidden_action_error_message) { ["missing delete permission"] }

        let(:sandbox_not_found_error_message) { ["Listing sandboxes not supported."] }

        let(:invalid_cookbook_version_error_message ) { ["Invalid cookbook version '#{cookbook_version}'."] }

      end
    end
  end
end
