#
# -*- indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
# ex: ts=4 sw=4 et
#
# Author:: Douglas Triggs (<doug@opscode.com>)
# Copyright:: Copyright (c) 2012 Opscode, Inc.
#

describe "Principals API Endpoint", :principals do

  # This code does not use any of the new hotness, and is in fact
  # somewhat temporary, so it has been decided not to waste the effort
  # on refactoring it.  The reason for this is that we need to verify
  # that the principal endpoint functions properly in OPC, as
  # currently the only consumer of this endpoint is pushy which
  # absolutely requires the OPC principal endpoint to work (as pushy
  # needs it for its own authentication, and pushy currently only runs
  # in OPC).  At the moment, the OSC tests can't simply be run in OPC
  # (the standard test functions, e.g., should_not_allow_method et
  # al., don't handle OPC properly).

  # When we do put more effort into generalizing pedant so that it
  # does proper authentication tests against OPC, all of this code can
  # probably be removed; other than auth, this endpoint works the same
  # in both environments.

  # At any rate, when that glorious day arrives, this code can be
  # nuked with mild prejudice.

  let(:principal_client_name) { platform.non_admin_client.name }
  let(:principal_user_name) { normal_user.name }
  let(:non_existent_principal_name) { 'not_a_number' }

  let(:failed_to_authenticate_as_invalid_msg) {
    ["Failed to authenticate as 'invalid'. Ensure that your node_name and client key are correct."] }
  let(:outside_user_not_associated_msg) {
    ["'pedant-nobody' not associated with organization '#{org}'"] }
  let(:cannot_load_nonexistent_msg) { 
    ["Cannot load client #{non_existent_principal_name}"] }
  let(:client_body) {
    {
      "name" => principal_client_name,
      "type" => "client",
      "public_key" => /^-----BEGIN CERTIFICATE-----/,
      "authz_id" => /.+/
    } }
  let(:user_body) {
    {
      "name" => principal_user_name,
      "type" => "user",
      "public_key" => /^-----BEGIN CERTIFICATE-----/,
      "authz_id" => /.+/
    } }

  describe 'access control' do
    context 'GET /principals', :pending do
      it 'returns a 401 ("Unauthorized") for invalid user' do
        get(api_url("/principals"),
            invalid_user) do |response|
          response.
            should look_like({
                               :status => 401,
                               :body_exact => {
                                 "error" => failed_to_authenticate_as_invalid_msg
                               }
                             })
        end
      end

      it 'returns a 403 ("Forbidden") for outside user' do
        get(api_url("/principals"),
            outside_user) do |response|
          response.should look_like({
                                      :status => 403,
                                      :body_exact => {
                                        "error" => outside_user_not_associated_msg
                                      }
                                    })
        end
      end
    end

    context 'GET /principals/<name>' do
      context 'when requesting a client' do
        it 'returns a 200 ("OK") for admin' do
          get(api_url("/principals/#{principal_client_name}"),
              admin_user) do |response|
            response.should look_like({
                                        :status => 200,
                                        :body_exact => client_body
                                      })
          end
        end

        it 'returns a 200 ("OK") for normal user' do
          get(api_url("/principals/#{principal_client_name}"),
              normal_user) do |response|
            response.should look_like({
                                        :status => 200,
                                        :body_exact => client_body
                                      })
          end
        end

        it 'returns a 200 ("OK") for invalid user' do
          # Currently, there is no auth on this endpoint at all; in the future,
          # these tests will have to change

          get(api_url("/principals/#{principal_client_name}"),
              invalid_user) do |response|
            response.should look_like({
                                        :status => 200,
                                        :body_exact => client_body
                                      })
          end
        end

        it 'returns a 200 ("OK") for outside user' do
          get(api_url("/principals/#{principal_client_name}"),
              outside_user) do |response|
            response.should look_like({
                                        :status => 200,
                                        :body_exact => client_body
                                      })
          end
        end
      end

      context 'when requesting a user' do
        it 'returns a 200 ("OK") for admin' do
          get(api_url("/principals/#{principal_user_name}"),
              admin_user) do |response|
            response.should look_like({
                                        :status => 200,
                                        :body_exact => user_body
                                      })
          end
        end

        it 'returns a 200 ("OK") for normal user' do
          get(api_url("/principals/#{principal_user_name}"),
              normal_user) do |response|
            response.should look_like({
                                        :status => 200,
                                        :body_exact => user_body
                                      })
          end
        end

        it 'returns a 200 ("OK") for invalid user' do
          get(api_url("/principals/#{principal_user_name}"),
              invalid_user) do |response|
            response.should look_like({
                                        :status => 200,
                                        :body_exact => user_body
                                      })
          end
        end

        it 'returns a 200 ("OK") for outside user' do
          get(api_url("/principals/#{principal_user_name}"),
              outside_user) do |response|
            response.should look_like({
                                        :status => 200,
                                        :body_exact => user_body
                                      })
          end
        end
      end

      it 'returns a 404 ("Not Found") for missing principal for admin' do
        get(api_url("/principals/#{non_existent_principal_name}"),
            admin_user) do |response|
          response.should look_like({
                                      :status => 404,
                                      :body_exact => {
                                        "error" => cannot_load_nonexistent_msg
                                      }
                                    })
        end
      end

      it 'returns a 404 ("Not Found") for missing principal for normal user' do

        get(api_url("/principals/#{non_existent_principal_name}"),
            normal_user) do |response|
          response.should look_like({
                                      :status => 404,
                                      :body_exact => {
                                        "error" => cannot_load_nonexistent_msg
                                      }
                                    })
        end
      end
    end
  end
end
