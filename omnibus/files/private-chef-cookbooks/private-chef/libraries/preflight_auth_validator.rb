#
# Copyright:: Copyright (c) 2016 Chef Software, Inc.
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

require_relative "preflight_checks"

class AuthPreflightValidator < PreflightValidator
  def initialize(node)
    super
    @user_ldap = PrivateChef['ldap']
  end

  def run!
    # Sanity check: for any configuration we should expect that
    # both LDAP and SAML auth cannot be enabled simultaneously
    # across Chef Server and Chef Manage
    validate_sane_state
  end

  private

  def validate_sane_state
    # used for checking if LDAP and SAML are both enabled
    chef_manage = { "chef_manage" => {} }
    if ::File.exists?("/var/opt/chef-manage/etc/chef-manage-running.json")
      chef_manage = JSON.parse(IO.read("/var/opt/chef-manage/etc/chef-manage-running.json"))
    end

    # Do a sanity check to make sure both SAML and LDAP are not enabled at the same time
    ldap_enabled = !(@user_ldap.nil? || @user_ldap.empty?)
    saml_enabled = chef_manage["chef-manage"] && chef_manage["chef-manage"]['saml'] && chef_manage["chef-manage"]['saml']['enabled']

    if ldap_enabled && saml_enabled
      fail_with err_AUTH001_failed_validation
    end
  end

  def err_AUTH001_failed_validation
<<EOM
AUTH001: Your configuration indicates that you are attempting to
         enable both LDAP and SAML authentication. Only one of these
         authentication types can be enabled across Chef Server and
         Chef Manage simultaneously.
EOM
  end

end
