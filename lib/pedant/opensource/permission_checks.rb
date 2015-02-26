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

require 'pedant/concern'

module Pedant
  module OpenSource
    module PermissionChecks
      extend Pedant::Concern
      # Shared contexts for creating permission-based testing of Open Source
      # endpoints

      shared_context 'permission checks' do
        let(:admin_response){fail "Please set an admin_response in the 'permission checks' shared context"}
        let(:non_admin_response){fail "Please set a 'non_admin_response' in the 'permission checks' shared context"}
        # Validators should be forbidden from doing anything except creating other clients
        let(:validator_response){forbidden_response}
        # In general, a bad client is ALWAYS unauthorized
        let(:bad_client_response){unauthorized_response}

        let(:validator_client){ Pedant::Client.new(platform.validator_client_name, "/etc/chef-server/chef-validator.pem")}

        context 'as an administrator' do
          let(:requestor) { admin_user }
          it { should look_like admin_response }
        end
        context 'as a non-administrator', :authorization do
          let(:requestor) { normal_user }
          it { should look_like non_admin_response }
        end
        context 'as a validator client', :authorization do
          let(:requestor) { validator_client }
          it { should look_like validator_response }
        end
        context 'as a bad client', :authorization do
          let(:requestor) { outside_user }
          it { should look_like bad_client_response }
        end
      end # checks permissions



      # DEPRECATED: USE should_not_allow_method INSTEAD!!!

      # Some operations don't work on certain endpoints, and we'd like to
      # maintain active checks against that behavior (as opposed to just
      # omitting tests)
      #
      # If you want a more complete response (e.g., checking the
      # headers to verify the acceptable HTTP methods), just override
      # 'method_not_allowed' response when you include this context
      shared_context 'a disallowed method' do
        include_context 'permission checks' do
          let(:admin_response){method_not_allowed_response}
          let(:non_admin_response){method_not_allowed_response}
          let(:validator_response){method_not_allowed_response}
          let(:bad_client_response){method_not_allowed_response}
        end
      end

      module ClassMethods
        def should_not_allow_method(http_method, _request_path = nil)
          context [http_method, _request_path].compact.join(" ") do
            let(:request_method) { http_method }
            let(:request_url)    { api_url _request_path } if _request_path

            include_context 'permission checks' do
              let(:admin_response)      { method_not_allowed_response }
              let(:non_admin_response)  { method_not_allowed_response }
              let(:validator_response)  { method_not_allowed_response }
              let(:bad_client_response) { method_not_allowed_response }
            end
          end
        end
      end

    end
  end
end
