# Copyright: Copyright (c) 2014 Opscode, Inc.
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

describe "status endpoint" do

  # TODO: this is hardcoded; this should be configured in the pedant config
  let(:request_url) { "http://127.0.0.1:8000/_status" }

  context 'GET _status' do
    let(:response_body) { {
        "status" => "pong",
        "upstreams" => {
          "chef_solr" => "pong",
          "chef_sql" => "pong",
          "chef_otto" => "pong",
          "oc_chef_authz" => "pong"
        }
      }}

    context "superuser", :smoke do
      it "can get status" do
        get(request_url, platform.superuser, :auth_headers => {}).should look_like({
            :status => 200,
            :body_exact => response_body
          })
      end
    end

    context "normal user" do
      it "can get status" do
        get(request_url, platform.non_admin_user, :auth_headers => {}).should look_like({
            :status => 200,
            :body_exact => response_body
          })
      end
    end

    context "invalid user" do
      it "can get status" do
        get(request_url, invalid_user, :auth_headers => {}).should look_like({
            :status => 200,
            :body_exact => response_body
          })
      end
    end

  end # context 'GET /_status'

  context 'POST _status' do
    let(:request_body) { {} }

    it "returns method not allowed" do
      delete(request_url, platform.superuser, :payload => request_body,
        :auth_headers => {}).should look_like({
          :status => 405
        })
    end

  end # context 'GET /_status'

  context 'PUT _status' do
    let(:request_body) { {} }

    it "returns method not allowed" do
      delete(request_url, platform.superuser, :payload => request_body,
        :auth_headers => {}).should look_like({
          :status => 405
        })
    end

  end # context 'GET /_status'

  context 'DELETE _status' do

    it "returns method not allowed" do
      delete(request_url, platform.superuser, :auth_headers => {}).should look_like({
          :status => 405
        })
    end

  end # context 'GET /_status'
end # describe "/_status endpoint"
