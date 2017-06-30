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

  ################################################################################
  # URL Generation and Authorized HTTP Request Helpers
  ################################################################################
  module Request
    require 'rest_client'
    require 'mixlib/shellout'
    include Pedant::JSON

    # TODO: alternative suggestions?
    # These accessors will at least hide the fact we're using a global...
    def server_api_version= (v)
      $server_api_version = v || Pedant::Config.server_api_version
    end
    def server_api_version
      $server_api_version
    end

    # Grab the the version of Chef / Knife that's on the box in order
    # to properly set the X-Chef-Version header
    KNIFE_VERSION = begin
                      require 'chef/version'
                      Chef::VERSION
                    rescue LoadError
                      # Don't want Bundler to poison the shelling out :(
                      cmd = Mixlib::ShellOut.new("knife --version", :environment => {
                                                   'BUNDLE_GEMFILE' => nil,
                                                   'BUNDLE_BIN_PATH' => nil,
                                                   'GEM_PATH' => nil,
                                                   'GEM_HOME' => nil,
                                                   'RUBYOPT' => nil
                                                 })
                      cmd.run_command
                      cmd.stdout =~ /^Chef: (.*)$/
                      $1 || raise("Cannot determine Chef version from output of `knife --version`: #{cmd.stdout}")
                    end

    # Headers that are added to all requests
    def standard_headers
      {
        'Accept' => 'application/json',
        'Content-Type' => 'application/json',
        'User-Agent' => 'chef-pedant rspec tests',
        'X-Chef-Version' => KNIFE_VERSION
      }
    end

    # Execute an authenticated request against a Chef Server
    #
    # `method` is an HTTP verb, as an uppercase symbol, e.g., :GET
    #
    # `url` is the complete URL for the request
    #
    # `requestor` is an instance of Pedant::Requestor, and represents
    # the user or client that the request will be signed as.  This
    # object actually generates the signing headers for the request.
    #
    # `opts` is a hash of options that modify the request in some way.
    # The currently recognized keys and their effects are as follows:
    #
    # :headers => any additional headers you wish to have applied to the
    # request.  A collection of standard headers are applied to all
    # requests, but any ones specified here will supercede those (see
    # the `standard_headers` shared context method).  Note that
    # authentication headers are applied last, and thus have priority
    # over any headers set in this hash.
    #
    # :server_api_version => allows you to specify server api version to
    # use for the request. Set to nil if you want to omit the default
    # version header from the request.
    #
    # :payload => the body of the request.  This is required for all PUT
    # and POST requests.  It should be given in its final form (i.e., as
    # a String, not a Ruby hash or anything else)
    #
    # :timestamp => the time of request signing.  If not supplied, the
    # current time is used.  This allows you to validate proper behavior
    # with expired requests.
    #
    # :auth_headers => the authorization headers to use (if any).
    #
    #
    # Finally, a block can be supplied to this method.  This block will
    # receive a single argument, the HTTP response (as a
    # RestClient::Response object).  Testing methods should use this to
    # carry out any validation tests of the response.
    def authenticated_request(method, url, requestor, opts={}, &validator)
      headers, payload = construct_request(method, url, requestor, opts)
      do_request(method, url, headers, payload, &validator)
    end

    # Construct an authenticated request against a Chef Server
    def construct_request(method, url, requestor, opts={})
      user_headers = opts[:headers] || {}
      version = opts[:server_api_version]
      payload_raw = opts[:payload] || ""

      payload = if payload_raw.class == Hash
                  to_json(payload_raw)
                else
                  payload_raw
                end

      # Provide a means to explicitly delete version header for test purposes
      version_headers = if opts.has_key?(:api_version)
                          version = opts[:api_version]
                          if version.nil?
                            {}
                          else
                            {"X-Ops-Server-API-Version" =>  version}
                          end
                        else
                          {"X-Ops-Server-API-Version" => server_api_version}
                        end

      auth_headers = opts[:auth_headers] || requestor.signing_headers(method, url, payload)

      uri = URI.parse(url)
      if (uri.scheme == 'http' && uri.port == 80) || (uri.scheme == 'https' && uri.port == 443)
        host = uri.host
      else
        host = "#{uri.host}:#{uri.port}"
      end

      final_headers = standard_headers.
        merge(auth_headers).
        merge(user_headers).
        merge(version_headers).
        merge({'Host' => host})

      [final_headers, payload]
    end

    def do_request(method, url, final_headers, payload, &validator)

      response_handler = lambda{|response, request, result| response}

      response = RestClient::Request.execute(method: method,
                                             url: url,
                                             payload: [:PUT, :POST].include?(method) ? payload : nil,
                                             headers: final_headers,
                                             ssl_version: Pedant::Config.ssl_version,
                                             verify_ssl: false,
                                             &response_handler)

      if block_given?
        yield(response)
      else
        response
      end
    end

    # Accessory methods for making requests a bit easier

    def get(url, requestor, opts={}, &validator)
      authenticated_request :GET, url, requestor, opts, &validator
    end

    def put(url, requestor, opts={}, &validator)
      authenticated_request :PUT, url, requestor, opts, &validator
    end

    def post(url, requestor, opts={}, &validator)
      authenticated_request :POST, url, requestor, opts, &validator
    end

    def delete(url, requestor, opts={}, &validator)
      authenticated_request :DELETE, url, requestor, opts, &validator
    end

    # Use in conjunction with the server_api_version accessor to
    # reset version to default
    def reset_server_api_version
      $server_api_version  = Pedant::Config.server_api_version
    end
    def use_max_server_api_version
      $server_api_version  = 1 # TODO Pedant::Config.max_server_api_version
    end

  end
end
