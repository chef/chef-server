#
# -*- indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
# ex: ts=4 sw=4 et
#
# Author:: Douglas Triggs (<doug@chef.io>)
# Copyright:: Copyright 2012-2018 Chef Software, Inc.
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
  let(:non_existent_org) { 'bogus-org' }

  let(:failed_to_authenticate_as_invalid_msg) {
    ["Failed to authenticate as 'invalid'. Ensure that your node_name and client key are correct."] }
  let(:outside_user_not_associated_msg) {
    ["'pedant-nobody' not associated with organization '#{org}'"] }
  let(:cannot_load_nonexistent_msg) {
    "Cannot find principal #{non_existent_principal_name}" }
  let(:cannot_load_org_msg) {
    "Cannot find org #{non_existent_org}" }
  let(:client_body) {
    {
      "name" => principal_client_name,
      # We're not actually validating the key, all we're testing here is that it
      # exists in the response body; if it's returning invalid keys, that would be
      # obvious from pushy test failures (possibly reporting as well).
      "public_key" => /^-----BEGIN (RSA )?PUBLIC KEY-----/,
      "type" => "client",
      "authz_id" => /^[0-9a-f]{32}$/,
      "org_member" => true
    } }
  let(:user_body) {
    {
      "name" => principal_user_name,
      # See above
      "public_key" => /^-----BEGIN (RSA )?PUBLIC KEY-----/,
      "type" => "user",
      "authz_id" => /^[0-9a-f]{32}$/,
      "org_member" => true
    } }

  describe 'access control' do
    context 'with bogus org' do
      it 'returns a 404 ("Not Found") for admin' do
        get("#{server}/organizations/#{non_existent_org}/principals/#{principal_client_name}",
            admin_user) do |response|
          response.
            should look_like({
              :status => 404,
              :body => {
                "error" => cannot_load_org_msg,
                "not_found" => "org"}
            })
        end
      end
      it 'returns a 404 ("Not Found") for normal user' do
        get("#{server}/organizations/#{non_existent_org}/principals/#{principal_client_name}",
            normal_user) do |response|
          response.
            should look_like({
              :status => 404,
              :body => {
                "error" => cannot_load_org_msg,
                "not_found" => "org"
              }
            })
        end
      end
    end

    context 'GET /principals' do
      # No client supplied = 404
      it 'returns a 404 ("Not Found") for admin' do
        get(api_url("/principals/"), admin_user) do |response|
          response.
            should look_like({ :status => 404 })
        end
      end
      it 'returns a 404 ("Not Found") for normal user' do
        get(api_url("/principals/"), normal_user) do |response|
          response.
            should look_like({ :status => 404 })
        end
      end
      it 'returns a 404 ("Not Found") for invalid user' do
        get(api_url("/principals/"), invalid_user) do |response|
          response.
            should look_like({ :status => 404 })
        end
      end

      it 'returns a 404 ("Not Found") for outside user' do
        get(api_url("/principals/"), outside_user) do |response|
          response.should look_like({ :status => 404 })
        end
      end
    end

    shared_context 'GET /principals/<name>' do
      context 'when requesting a client' do
        it 'returns a 200 ("OK") for admin' do
          get(api_url("/principals/#{principal_client_name}"), admin_user) do |response|
            response.should look_like({
                :status => 200,
                :body_exact => versioned_client_body
              })
          end
        end

        it 'returns a 200 ("OK") for normal user' do
          get(api_url("/principals/#{principal_client_name}"), normal_user) do |response|
            response.should look_like({
                :status => 200,
                :body_exact => versioned_client_body
              })
          end
        end

        it 'returns a 200 ("OK") for invalid user' do
          # Currently, there is no auth on this endpoint at all; in the future,
          # these tests will have to change

          get(api_url("/principals/#{principal_client_name}"), invalid_user) do |response|
            response.should look_like({
                :status => 200,
                :body_exact => versioned_client_body
              })
          end
        end

        it 'returns a 200 ("OK") for outside user' do
          get(api_url("/principals/#{principal_client_name}"), outside_user) do |response|
            response.should look_like({
                :status => 200,
                :body_exact => versioned_client_body
              })
          end
        end
      end

      context 'when requesting a user' do
        it 'returns a 200 ("OK") for admin' do
          get(api_url("/principals/#{principal_user_name}"), admin_user) do |response|
            response.should look_like({
                :status => 200,
                :body_exact => versioned_user_body
              })
          end
        end

        it 'returns a 200 ("OK") for normal user' do
          get(api_url("/principals/#{principal_user_name}"), normal_user) do |response|
            response.should look_like({
                :status => 200,
                :body_exact => versioned_user_body
              })
          end
        end

        it 'returns a 200 ("OK") for invalid user' do
          get(api_url("/principals/#{principal_user_name}"), invalid_user) do |response|
            response.should look_like({
                :status => 200,
                :body_exact => versioned_user_body
              })
          end
        end

        it 'returns a 200 ("OK") for outside user' do
          get(api_url("/principals/#{principal_user_name}"), outside_user) do |response|
            response.should look_like({
                :status => 200,
                :body_exact => versioned_user_body
              })
          end
        end
      end

      context 'when requesting an outside user', :multiuser do
        let(:principal_user_name) { outside_user.name }
        let(:user_body) {
          {
            "name" => principal_user_name,
            "public_key" => /^-----BEGIN (RSA )?PUBLIC KEY-----/,
            "type" => "user",
            "authz_id" => /^[0-9a-f]{32}$/,
            "org_member" => false
          } }

        it 'returns a 200 ("OK") for admin' do
          get(api_url("/principals/#{principal_user_name}"), admin_user) do |response|
            response.should look_like({
                :status => 200,
                :body_exact => versioned_user_body
              })
          end
        end

        it 'returns a 200 ("OK") for normal user' do
          get(api_url("/principals/#{principal_user_name}"), normal_user) do |response|
            response.should look_like({
                :status => 200,
                :body_exact => versioned_user_body
              })
          end
        end

        it 'returns a 200 ("OK") for invalid user' do
          get(api_url("/principals/#{principal_user_name}"), invalid_user) do |response|
            response.should look_like({
                :status => 200,
                :body_exact => versioned_user_body
              })
          end
        end

        it 'returns a 200 ("OK") for outside user' do
          get(api_url("/principals/#{principal_user_name}"), outside_user) do |response|
            response.should look_like({
                :status => 200,
                :body_exact => versioned_user_body
              })
          end
        end
      end

      context 'when requesting a bad client' do
        let(:principal_client_name) { platform.bad_client.name }
        let(:cannot_load_nonexistent_msg) {
          "Cannot find principal #{principal_client_name}" }

        it 'returns a 404 ("Not Found") for admin' do
          get(api_url("/principals/#{principal_client_name}"), admin_user) do |response|
            response.should look_like({
                :status => 404,
                :body_exact => {
                  "error" => cannot_load_nonexistent_msg,
                  "not_found" => "principal"
                }
              })
          end
        end

        it 'returns a 404 ("Not Found") for normal user' do
          get(api_url("/principals/#{principal_client_name}"), normal_user) do |response|
            response.should look_like({
                :status => 404,
                :body_exact => {
                  "error" => cannot_load_nonexistent_msg,
                  "not_found" => "principal"
                }
              })
          end
        end

        it 'returns a 404 ("Not Found") for invalid user' do
          get(api_url("/principals/#{principal_client_name}"), invalid_user) do |response|
            response.should look_like({
                :status => 404,
                :body_exact => {
                  "error" => cannot_load_nonexistent_msg,
                  "not_found" => "principal"
                }
              })
          end
        end

        it 'returns a 404 ("Not Found") for outside user' do
          get(api_url("/principals/#{principal_client_name}"), outside_user) do |response|
            response.should look_like({
                :status => 404,
                :body_exact => {
                  "error" => cannot_load_nonexistent_msg,
                  "not_found" => "principal"
                }
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
                "error" => cannot_load_nonexistent_msg,
                "not_found" => "principal"
              }
            })
        end
      end

      it 'returns a 404 ("Not Found") for missing principal for normal user' do

        get(api_url("/principals/#{non_existent_principal_name}"), normal_user) do |response|
          response.should look_like({
              :status => 404,
              :body_exact => {
                "error" => cannot_load_nonexistent_msg,
                "not_found" => "principal"
              }
            })
        end
      end
    end

    context '[deprecated] v0', :api_v0 do
        let(:versioned_client_body) { client_body }
        let(:versioned_user_body) { user_body }
        include_context 'GET /principals/<name>'
    end

    context '[current] v > 0', :api_v1 do
        before(:all) do
            platform.use_max_server_api_version
        end
        after(:all) do
            platform.reset_server_api_version
        end
        let(:versioned_client_body) { {"principals" => [ client_body ]} }
        let(:versioned_user_body) { {"principals" => [ user_body ]} }
        include_context 'GET /principals/<name>'
    end
  end
end
