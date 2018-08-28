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

require 'pedant/rspec/common'
require 'pedant/platform'

module Pedant
  module RSpec
    module AuthHeadersUtil
      # rspec dsl is mixed into every module

      shared_context 'handles authentication headers correctly' do
        def digester
          Mixlib::Authentication::Digester
        end

        def self.digester
          Mixlib::Authentication::Digester
        end

        # Stolen shamelessly from Mixlib::Authentication::SignedHeaderAuth
        # Build the canonicalized request based on the method, other headers, etc.
        # compute the signature from the request, using the looked-up user secret
        # ====Parameters
        # private_key<OpenSSL::PKey::RSA>:: user's RSA private key.
        def manufacture_signed_headers(requestor, method, url, body, options={})
          # Calculate things and handle options
          body = "" if !body
          body = to_json(body) if body.class == Hash

          signing_description = options.include?(:signing_description) ?
                                  options[:signing_description] : "version=1.0"

          version = if signing_description =~ /version=([0-9\.]*)/
                      $1
                    else
                      "1.0"
                    end

          digest = if version == "1.3"
                     OpenSSL::Digest::SHA256
                   else
                     OpenSSL::Digest::SHA1
                   end

          user_id = options.include?(:user_id) ?
            options[:user_id] : requestor.name
          timestamp = options.include?(:timestamp) ?
            options[:timestamp] : Time.now.utc.iso8601
          hashed_body = options.include?(:hashed_body) ?
            options[:hashed_body] : digester.hash_string(body, digest)

          private_key = options.include?(:private_key) ?
            options[:private_key] : requestor.signing_key
          http_method = options.include?(:method) ?
            options[:method] : method
          path = options.include?(:path) ?
            options[:path] : URI.parse(url).path
          hashed_path = options.include?(:hashed_path) ?
            options[:hashed_path] : digester.hash_string(path, digest)

          header_hash = {}
          header_hash["X-Ops-Sign"] = signing_description if signing_description
          header_hash["X-Ops-Userid"] = user_id if user_id
          header_hash["X-Ops-Timestamp"] = timestamp if timestamp
          header_hash["X-Ops-Content-Hash"] = hashed_body if hashed_body

          server_api_version = "0"

          string_to_sign = if options[:string_to_sign]
                             options[:string_to_sign]
                           elsif version == "1.3"
                             "Method:#{http_method.to_s.upcase}\nPath:#{path}\n" +
                             "X-Ops-Content-Hash:#{hashed_body}\nX-Ops-Sign:version=#{version}\n" +
                             "X-Ops-Timestamp:#{timestamp}\n" +
                             "X-Ops-UserId:#{user_id}\n" +
                             "X-Ops-Server-API-Version:#{server_api_version}"
                           else
                             "Method:#{http_method.to_s.upcase}\nHashed Path:#{hashed_path}\n" +
                             "X-Ops-Content-Hash:#{hashed_body}\nX-Ops-Timestamp:#{timestamp}\n" +
                             "X-Ops-UserId:#{user_id}"
                           end

          signature = if options.include?(:signature)
                        options[:signature]
                      elsif version == "1.3"
                        Base64.encode64(private_key.sign(digest.new, string_to_sign)).chomp
                      else
                        Base64.encode64(private_key.private_encrypt(string_to_sign)).chomp
                      end

          if signature
            # lines - X-Ops-Authorization-1, ... (starts at 1, not 0!)
            signature_lines = signature.split(/\n/)
            signature_lines.each_index do |idx|
              if signature_lines[idx]
                key = "X-Ops-Authorization-#{idx + 1}"
                header_hash[key] = signature_lines[idx]
              end
            end
          end

          Mixlib::Authentication::Log.debug "String to sign: '#{string_to_sign}'\n" +
                                            "Header hash: #{header_hash.inspect}"
          header_hash
        end

        # This context takes in:
        # - method: the method of the request: :POST, :DELETE, :PUT or :GET
        # - url: the URL to send the request to
        # - payload: the payload (if any)
        # - response_should_be_successful: define this to check if
        #   the response is successful.  "response" will give you the
        #   response.
        # - success_user: a user (NOT superuser) who would
        #   normally succeed in making this request
        # - failure_user: a user who would normally fail in
        #   making this request

        # SIGNATURE HEADERS
        # X-Ops-Sign: Must be version=1.0
        # X-Ops-Userid: user who signed the request.
        # X-Ops-Timestamp: time request was signed (cannot be askew)
        # X-Ops-Content-Hash: hashed body of request
        # X-Ops-Authorization-\d+: 1-n lines of signature
        # X-Ops-Requesting-Actor-Id: name of user you want to impersonate (only
        #   superuser should be able to do this)
        #
        # Each of these needs tests for:
        # - Missing value
        # - Empty value
        # - Malformed value
        # - Well-formed but incorrect value
        #
        # Note that leading and trailing white space is not included
        # in header values by RFC2616. So the behavior of an empty
        # header value should match that of a missing header value.

        # Everything correct
        let(:response) do
          authenticated_request(method, url, user,
                                :payload => body,
                                :auth_headers => auth_headers)
        end

        def self.with_modified_auth_headers(desc, expected_status, auth_header_options)
          context_options = auth_header_options[:focus] ? [:focus] : []
          context_options += auth_header_options[:pending] ? [:pending] : []
          context_options += auth_header_options[:skip] ? [:skip] : []
          context_options << :validation if expected_status == 400
          context_options << :authentication if expected_status == 401
          context_options << :authorization if expected_status == 403
          context(desc, *context_options) do
            let(:auth_headers) { manufacture_signed_headers(user, method, url, body,
                                                            auth_header_options) }
            it "returns #{expected_status}" do
              response.should look_like({ :status => expected_status })
            end
          end
        end

        context "with signing protocol 1.3", :authentication do
          let(:user) { success_user }
          let(:expected_status) {
            if method == :POST
              201
            else
              200
            end
          }

          let(:auth_headers) {
            manufacture_signed_headers(user, method, url, body,
                                       :signing_description => "algorithm=sha256;version=1.3")
          }

          it "returns successfully" do
            response.should look_like({ :status => expected_status })
          end
        end

        context "with successful user", :authentication do
          let(:user) { success_user }
          context "with everything correct" do
            let(:auth_headers) { manufacture_signed_headers(user, method, url, body) }
            it 'is successful' do
              response_should_be_successful
            end
          end

          # X-Ops-Sign
          with_modified_auth_headers('missing X-Ops-Sign', 400, :signing_description => nil)
          with_modified_auth_headers('unsupported X-Ops-Sign version', 400,
                                     :signing_description => 'version=8.1')
          with_modified_auth_headers('empty X-Ops-Sign', 400, :signing_description => '')

          # X-Ops-Userid
          with_modified_auth_headers('missing X-Ops-Userid', 400, :user_id => nil,
                                     :skip => true)
          # an empty header should be treated as missing by the server
          with_modified_auth_headers('empty X-Ops-Userid', 400, :user_id => '',
                                     :skip => true)
          with_modified_auth_headers('nonexistent username in X-Ops-Userid', 401,
                                     :user_id => 'nowaythisexists')
          context "when X-Ops-Userid does not match signature", :authentication do
            let(:auth_headers) do
              manufacture_signed_headers(user, method, url, body)
              .merge({ 'X-Ops-Userid' => failure_user.name })
            end
            it 'returns 401' do
              response.should look_like({ :status => 401 })
            end
          end
          with_modified_auth_headers('absolutely immense X-Ops-Userid', 401, :user_id => 'x'*2000, :skip => true)

          # X-Ops-Timestamp
          with_modified_auth_headers('missing X-Ops-Timestamp', 400, :timestamp => nil)
          # empty should behave just like missing
          with_modified_auth_headers('empty X-Ops-Timestamp', 400,
                                     :timestamp => '')
          with_modified_auth_headers('malformed X-Ops-Timestamp', 401,
                                     :timestamp => 'xxx')
          with_modified_auth_headers('old X-Ops-Timestamp', 401,
                                     :timestamp => (Time.now.utc - 60*60).iso8601)

          with_modified_auth_headers('future X-Ops-Timestamp', 401,
                                     :timestamp => (Time.now.utc + 60*60).iso8601)

          context 'recent X-Ops-Timestamp' do
            let(:auth_headers) { manufacture_signed_headers(user, method, url, body,
                                                            :timestamp => (Time.now.utc - 30).iso8601) }
            it "succeeds" do
              response_should_be_successful
            end
          end
          context "when X-Ops-Timestamp does not match signature", :authentication do
            let(:auth_headers) do
              manufacture_signed_headers(user, method, url, body)
              .merge({ 'X-Ops-Timestamp' => (Time.now.utc - 30).iso8601 })
            end
            it 'returns 401' do
              response.should look_like({ :status => 401 })
            end
          end

          # X-Ops-Content-Hash: hashed body of request
          with_modified_auth_headers('missing X-Ops-Content-Hash', 400, :hashed_body => nil)
          with_modified_auth_headers('empty X-Ops-Content-Hash', 400, :hashed_body => '')
          with_modified_auth_headers('malformed X-Ops-Content-Hash', 401, :hashed_body => 'xxx')
          with_modified_auth_headers('when body does not match X-Ops-Content-Hash and signature', 401,
                                     :hashed_body => digester.hash_string('{thisisnotevenjson}'))
          context "when X-Ops-Content-Hash does not match body and signature", :authentication do
            let(:auth_headers) do
              manufacture_signed_headers(user, method, url, body)
              .merge({ 'X-Ops-Content-Hash' => digester.hash_string('{thisisnotevenjson}') })
            end
            it 'returns 401' do
              response.should look_like({ :status => 401 })
            end
          end

          # X-Ops-Authorization-\d+: 1-n lines of signature
          with_modified_auth_headers('missing signature in X-Ops-Authorization-', 401, :signature => nil)
          with_modified_auth_headers('malformed signature in X-Ops-Authorization-', 401, :signature => 'xxx')
          # Note that an empty signature doesn't really end up empty
          # in the request. This actually results in the empty string
          # being signed and base 64 encoded.
          with_modified_auth_headers('empty signature in X-Ops-Authorization-', 401, :signature => '')
          context 'missing line 1 in signature in X-Ops-Authorization-', :authentication do
            let(:auth_headers) do
              headers = manufacture_signed_headers(user, method, url, body)
              headers.delete('X-Ops-Authorization-1')
              headers
            end
            it 'returns 401' do
              response.should look_like({ :status => 401 })
            end
          end
          context 'missing a middle line in signature in X-Ops-Authorization-', :authentication do
            let(:auth_headers) do
              headers = manufacture_signed_headers(user, method, url, body)
              headers.delete('X-Ops-Authorization-2')
              headers
            end
            it 'returns 401' do
              response.should look_like({ :status => 401 })
            end
          end
          context 'missing the last line in signature in X-Ops-Authorization-', :authentication do
            let(:auth_headers) do
              headers = manufacture_signed_headers(user, method, url, body)
              signature_headers = headers.each_key.select { |key| key =~ /X-Ops-Authorization-/ }
              max_header = signature_headers.map { |key| key.split('-')[-1].to_i }.max
              headers.delete("X-Ops-Authorization-#{max_header}")
              headers
            end
            it 'returns 401' do
              response.should look_like({ :status => 401 })
            end
          end
          context 'with a different method in the signature', :authentication do
            let(:auth_headers) do
              manufacture_signed_headers(user, method, url, body,
                                         :method => (method == :GET ? :POST : :GET))
            end
            it 'returns 401' do
              response.should look_like({ :status => 401 })
            end
          end
          with_modified_auth_headers('with a different path in the signature', 401,
                                     :path => '/organizations/SomeoneElse/whatever')
          with_modified_auth_headers('with an empty decoded signature', 401,
                                     :string_to_sign => '')
          with_modified_auth_headers('with a malformed decoded signature', 401,
                                     :string_to_sign => 'XXX')
          context "when signature does not match body and X-Ops-Content-Hash", :authentication do
            let(:auth_headers) do
              # Make a different signature
              other_headers = manufacture_signed_headers(user, method, url,
                                                         '{ "noooo" => "kh11n" }')
              other_headers.delete_if { |key,value| key !~ /X-Ops-Authorization-/ }
              manufacture_signed_headers(user, method, url, body).merge(other_headers)
            end
            it 'returns 401' do
              response.should look_like({ :status => 401 })
            end
          end
        end

        # X-Ops-Request-Source
        context 'when X-Ops-Request-Source is web' do
          if Pedant::Config.pedant_platform.webui_key
            # If no webui_key defined (i.e., in pushy pedant) skip
            # these tests

            let(:auth_headers) do
              manufacture_signed_headers(user, method, url, body).
                merge({ 'X-Ops-Request-Source' => 'web' })
            end

            context 'impersonating successful user' do
              let(:user) { impersonate(success_user) }
              it 'succeeds' do
                response_should_be_successful
              end
            end

            context 'impersonating failed user', :authentication do
              let(:user) { impersonate(failure_user) }
              it 'fails' do
                response.should look_like forbidden_response
              end
            end
          else
            # But make sure to include pending tests, in case webui_key
            # missing is accidental

            context 'impersonating successful user' do
              it 'succeeds',
                :skip => 'no webui_key available in key store' do
                ;
              end
            end

            context 'impersonating failed user', :authentication do
              it 'fails',
                :skip => 'no webui_key defined in pedant config' do
                ;
              end
            end
          end
        end

        # X-Ops-Webkey-Tag
        context 'X-Ops-Webkey-Tag', :skip => "Write X-Ops-Webkey-Tag tests" do
        end

        context "with other successful user" do
          let (:user) { success_user }
          let (:auth_headers) { nil }
          it 'succeeds' do
            response_should_be_successful
          end
        end

        # TODO: Refactor this to work more generally across platforms
        # (do not assume user when it could be a client)
        #
        # context "with other failed user" do
        #   let (:user) { failure_user }
        #   let (:auth_headers) { nil }
        #   it 'fails with 403' do
        #     response.should look_like({ :status => 403 })
        #   end
        # end
      end
    end
  end
end
