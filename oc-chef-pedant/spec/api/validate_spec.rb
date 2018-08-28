#
# -*- indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
# ex: ts=4 sw=4 et
#
# Author:: Mark Anderson <mark@chef.io>
# Copyright:: Copyright 2016-2018 Chef Software, Inc.
#

describe "Validation API Endpoint", :authentication do
  let(:bad_client) { platform.bad_client }
  let(:test_body) {
    {foo: "bar"}
  }
  let(:user_body) {
    {"requestor_name" => normal_user.name,
     "requestor_type" => "user"}
  }

  let(:failed_to_authenticate_as_invalid_msg) {
    ["Failed to authenticate as 'invalid'. Ensure that your node_name and client key are correct."] }

  describe 'We make a request' do
    describe 'with a valid user' do
      describe 'and valid path' do
        let(:real_url) { platform.internal_api_url("nodes") }
        let(:redir_url) {
          uri_path = URI(real_url).path
          platform.internal_api_url("validate#{uri_path}")
        }

        [:GET, :POST, :PUT, :DELETE].each do |req|
          it "is a #{req}" do
            headers, payload = construct_request(req, real_url, normal_user, test_body)
            do_request(req, redir_url, headers, payload) do |response|
              response.should look_like({
                                          :status => 200,
                                          :body_exact => user_body
                                        })
            end
          end
        end
      end
    end
    describe 'with a valid client' do
      let(:client) { platform.non_admin_client }
      let(:client_body) {
        {"requestor_name" => client.name,
         "requestor_type" => "client"}
      }

      describe 'and valid path' do
        let(:real_url) { platform.internal_api_url("nodes") }
        let(:redir_url) {
          uri_path = URI(real_url).path
          platform.internal_api_url("validate#{uri_path}")
        }

        [:GET, :POST, :PUT, :DELETE].each do |req|
          it "is a #{req}" do
            headers, payload = construct_request(req, real_url, client, test_body)
            do_request(req, redir_url, headers, payload) do |response|
              response.should look_like({
                                          :status => 200,
                                          :body_exact => client_body
                                        })
            end
          end
        end
      end

      describe 'with the wrong path' do
        let(:real_url) { platform.internal_api_url("nodes") }
        let(:redir_url) {
          uri_path = URI(real_url).path
          redir_url = platform.internal_api_url("validate/dog")
        }

        [:GET, :POST, :PUT, :DELETE].each do |req|
          it "is a #{req}" do
            headers, payload = construct_request(:POST, real_url, client, test_body)
            do_request(:POST, redir_url, headers, payload) do |response|
              response.should look_like({
                                      :status => 401
                                        })
            end
          end
        end
      end
    end
    describe 'with a valid client not in the org' do
      it 'and valid path' do
        real_url = platform.internal_api_url("nodes")
        uri_path = URI(real_url).path
        redir_url = platform.internal_api_url("validate#{uri_path}")

        headers, payload = construct_request(:POST, real_url, bad_client, test_body)
        do_request(:POST, redir_url, headers, payload) do |response|
          response.should look_like({
                                      :status => 401
                                    })
        end
      end
    end
  end
end
