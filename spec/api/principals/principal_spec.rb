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

require 'pedant/rspec/principal_util'

describe "Open Source /principals endpoint", :principals => true, :platform => :open_source do
  include Pedant::RSpec::PrincipalUtil

  context 'GET /users' do
    #let(:request_method) { :GET }
    #let(:request_url)    { api_url '/principals' }
    #let(:requestor)      { superuser }

    context 'with no extra users' do
      # Endpoint isn't done yet

      # should_respond_with 200, 'and the Pedant-created users'
    end
  end

  context 'GET /principals/<name>' do
    let(:request_method) { :GET }
    let(:requestor)      { superuser }
    let(:expected_response) { ok_full_response }
    let(:request_url)    { api_url "/principals/#{principal_name}" }

    context "a regular user", :smoke do
      let(:principal_name) { user_name }
      let(:success_message) do
        {
          "name" => principal_name,
          "type" => "user",
          "public_key" => /^-----BEGIN PUBLIC KEY-----/
        }
      end

      should_respond_with 200, 'and the user'
    end

    context "a regular client", :smoke do
      let(:principal_name) { client_name }
      let(:success_message) do
        {
          "name" => principal_name,
          "type" => "client",
          "public_key" => /^-----BEGIN PUBLIC KEY-----/
        }
      end

      should_respond_with 200, 'and the client'
    end

    context "a non-existent user" do
      let(:principal_name) { nonexistent_principal_name }

      it "is not found" do
        should look_like principal_not_found_response
      end
    end
  end # context 'GET /users/<name>'
end
