#
# Copyright:: Copyright (c) 2017 Chef Software, Inc.
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

require_relative './preflight_checks.rb'

class SslPreflightValidator < PreflightValidator
  def initialize(node)
    super
    @user_config = PrivateChef['nginx']
  end

  def run!
    verify_fips_sanity
    verify_cert_pair
  end

  def verify_fips_sanity
    if PrivateChef['fips'] && !fips_supported_ssl?
      fail_with <<EOF
You have enabled FIPS-mode in chef-server.rb but FIPS does not appear
to be supported on this platform.

#{openssl_exe} reported its version as:

    #{openssl_version}

which does not contain the expected -fips identifier.
EOF
    end
  end

  def openssl_exe
    '/opt/opscode/embedded/bin/openssl'
  end

  def openssl_version
    @openssl_version ||= begin
                           `#{openssl_exe} version`
                         rescue
                           "unknown"
                         end
  end

  def fips_supported_ssl?
    case openssl_version
    when /^unknown/
      Chef::Log.warn("Failed to parse openssl version, assuming it would have supported FIPS")
      # We could report false here if we wanted to be pessimistic
      true
    when /OpenSSL .*-fips/
      true
    else
      false
    end
  end

  def verify_cert_pair
    cert = @user_config['ssl_certificate']
    key  = @user_config['ssl_certificate_key']
    if !cert.nil? && key.nil?
        fail_with <<EOF
Your configuration has specified

nginx['ssl_certificate'] = "#{cert}"

but has not specified nginx['ssl_certificate_key'].  You must specify
both configuration options or neither.
EOF
    elsif cert.nil? && !key.nil?
        fail_with <<EOF
Your configuration has specified

nginx['ssl_certificate_key'] = "#{key}"

but has not specified nginx['ssl_certificate'].  You must specify
both configuration options or neither.
EOF
    end
  end
end
