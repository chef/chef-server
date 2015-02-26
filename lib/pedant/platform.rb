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

require 'uri'
require 'pathname'

module Pedant

  # Abstraction of an Opscode Chef Server platform
  class Platform
    include Pedant::Request

    attr_reader :server, :superuser, :superuser_key_file

    # Create a Platform object for a given server (specified by
    # protocol, hostname, and port ONLY).  You must supply the
    # superuser's key file, which can either be the path to the file,
    # or the contents of the file.
    def initialize(server, superuser_key_file, superuser_name)
      @server = (Pedant.config.explicit_port_url ? explicit_port_url(server) : server )
      puts "Configured URL: #{@server}"
      @superuser_key_file = superuser_key_file
      @superuser = Pedant::Requestor.new(superuser_name, superuser_key_file, platform: self)
      self.pedant_run_timestamp # Cache the global timestamp at initialization
      self
    end

    def api_url(url_path)
      raise "Must override with an appropriate url generation method!"
    end

    def org_name
      path = Pathname(URI(server).path)
      if !path.root? && !path.parent.root? && path.parent.basename.to_s == 'organizations'
        path.basename.to_s
      else
        'chef'
      end
    end

    def validator_client_name
      "#{org_name}-validator"
    end

    def admin_client_name
      "#{org_name}-webui"
    end

    # Since Erchef will now return URLs based upon the Host: header, and it receives
    # a fully-qualified url with an explicit port, we need to normalize the server url
    # so that port numbers are added, even if the url are default port
    def explicit_port_url(url)
      uri = URI.parse(url)
      return url unless uri.default_port == uri.port
      return "#{url}:#{uri.port}"
    end

    def setup
      raise "Must override with an appropriate setup method!"
    end

    def cleanup
      raise "Must override with an appropriate cleanup method!"
    end

    def test_repository_path
      @_test_respository_path ||= Pedant::Utility.fixture_path('test_repository')
    end

    def requestor_cache
      @_requestor_cache ||= {}
    end

    # Generate a symbol key for inserting / retrieving a requestor
    # from the requestor cache.  +config_key+ should be the key under
    # which a requestor hash is stored in the Pedant config, while
    # +type+ should be either :user or :client.
    #
    # For example, if you have, say, a config entry like
    #
    #    Pedant.config['requestors']['clients']['admin']
    #
    # you would pass in 'admin' for +config_key+.
    def cache_key(config_key, type)
      valid_types = [:user, :client]
      unless valid_types.include?(type)
        raise "Can only create a cache key for #{valid_types}; you gave #{type.inspect}"
      end
      "#{config_key}_#{type}".to_sym
    end

    # Generate an accessor method to easily get a requestor that has
    # been generated from a Pedant config file.  The +cache_key+
    # should be generated from the +cache_key+ method.
    #
    # For example, if you have, say, a config entry like
    #
    #    Pedant.config['requestors']['clients']['admin']
    #
    # then you will be able to access that user via
    #
    #    platform.admin_client
    #
    # Similar logic holds for users as well.
    def create_requestor_accessor(cache_key)
      self.class.send(:define_method, cache_key, lambda{requestor_cache[cache_key]})
    end

    def cleanup_requestors
      # Don't need to delete bogus clients, because it wasn't
      # associated with the server to begin with
      requestor_cache.values.reject(&:bogus?).each(&:delete!)
    end

    def users
      requestor_cache.values.select { |r| Pedant::User === r }
    end

    def clients
      requestor_cache.values.select { |r| Pedant::Client === r }
    end

    # Well-formed key, but not actually associated with the server
    def bogus_key
      @_bogus_key ||=
"-----BEGIN RSA PRIVATE KEY-----
MIIEogIBAAKCAQEA7TfhToresZudM5gaBfzM/eHrGuJtN8uMaG51fLk9rNVwOxIw
eb9AmWJSQgftRj4AJSnt8Jv+QyAafjg79rEmCpd2K+toN1fXiVsf/ld/VdMI5vCd
usJc8aC4OFIVrLetm9eq+joXiCSLfEYP1w4d1gc9wqr54rnGVgQdWv31NhqXX7Tl
2bHRoEsfIFM7Vr5zC9Rv6XEtrwezSpzQ+ru707UOCOHNnH1TDxNDz+L71xJnGXD6
uh596hVm2GiXJ8lyCVJTs/RZ+HKtRuxYOJztdCQAXbd6DDjTBax/cCEzzoxK/LMc
1K/zWCKN8yM/XpiFqNuBB0yrqS5Y4zlROY0ssQIDAQABAoIBAQDmC1HYvD1YGePq
O+/IrK8S6jr4WGq4OBIS2EPhTzbrXBU5g9s0xe7ckIfa9xr4CnpTkATqWCzMZd6r
Vtd31bVhgh6cWu829F3WG2O8YJfg4AX7B46+pWxC+qyMGbZhR8L5pb1ualWVtnL6
cms8D7mJbH5NQUeRwrz/f4AEVNGuw165PhglXnRq3zxs97jdNH6av0na1VLAmUeR
vmYToLoIsK5yLeNzMfRRrQkq4ndUvad2ahqsz+p/xUjfYneBz4uujE/PcCgi423Q
FMH1NtPpzmuZuABU53wUMW4BXsSGv+34UMhRTazcBPRBYjKT6l6F7Tt9wPFxrtVn
KYI9dI0tAoGBAPzBwLsUuyEJtSd2zW63f7EhhG/2rSBm1g9Bcs1sHZtXIX3JN51X
hd+ckXQcLgQYn8syGW1+WcuaHGyp7v3G0yr6b38FWwEnmD85Mq5da1dyio2udBdK
Xm8MfY94yP8qSH3bUDKEl+cV9X5rtzbQAorMXb/Qkc4vErWfABVBKeLPAoGBAPBD
FjGxER9YU4hFV2vn5qElfxa799Vl0uSvao0lSkTpA/sHFxAkRQc/mo1PBClaH0ba
Hqr141o5pGUDgLqpO3kEY5vYBOaFXLFdFCcL+1YUR6t0bX+WCHuq21Cs6Gu4+qNA
D4dpsSPDXfatyXWM5PF2d4FwO2XnL25Yt+rg6dh/AoGAbEfk9UCQWjr6bImswH3E
KnIddonK6VKk6aw0LmTe2imdo3GMbc+M/prohUF9RSv3aOlxk0LJ3TuMadDzHa0L
0iGvmk8FCZ2Yz50FZUWIMtJTIRdXjJLDmfdT4x7vnMDUhXZrCPlcyhbSMPKcbtL2
A9hBYWdMz3PDJCOVuYVNGGkCgYEAhSxKUwTYfs1Qj8oPqOoDdfL4vLs3pfsoqEVr
BA1VW1jlMfE+IV5ZPKlOm2De56TijT09nnloqYwlyS/l3JENPAjoxWs5XCUzucPj
9bi4eYAIMcr5Hq0kypdrtQ4CTiNcGbzaXq6A11fk72RotFWCWSzXFNIGuncoXTuj
xfcg5zUCf3TQrwpcBB1Hf9hm6lUVpptvUZJAQtkWoaWXUPRY0CjcVdDc5ak4xqL2
FcAuJwV95b3qLyPESJ8zUBLOg3DcT1X9mrvRuKgC/Ie6k4R+oxU/nXi4vsPXlvg4
yap6MUYSjPOa7eCrhg2zFZiqO6VLEogPc1nsjb9Zl2UWLLYyCVz=
-----END RSA PRIVATE KEY-----"
    end

    def dummy_client(h)
      Pedant::Client.new(h[:name], bogus_key, platform: self, preexisting: false, bogus: true)
    end

    # Normalize timestamps used in Pedant
    # In tests, you can access this with:
    # let(:timestamp) { platform.now }
    def timestamp
      Time.now.utc
    end

    alias now timestamp

    # Global timestamp marking the beginning of the run
    # Use this for things like generating unique names
    def pedant_run_timestamp
      @_pedant_run_timestamp ||= self.timestamp
    end
  end
end
