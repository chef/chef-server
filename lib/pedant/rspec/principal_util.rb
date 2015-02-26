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

require 'pedant/request'
require 'rspec/core/shared_context'

module Pedant
  module RSpec
    module PrincipalUtil
      extend ::RSpec::Core::SharedContext

      # TODO: Open-Source Chef pretends that "admin_user" is a user
      # when it is a client Now that OSC is getting real user
      # endpoints, then this should be fixed across board.  Ideally,
      # all tests should run with both clients and users as
      # requestors.
      let(:user_name){platform.admin_user.name}
      let(:client_name){platform.validator_client_name}
      let(:nonexistent_principal_name){"non-existent"}

      let(:principal_not_found_response) { resource_not_found_response }

    end
  end
end
