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
    verify_cert_pair
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
