#
# Copyright:: Copyright (c) 2015 Chef Software, Inc.
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

class BootstrapPreflightValidator < PreflightValidator
  def initialize(node)
    super
  end

  def run!
    # Sanity checks: for any configuration we should expect that
    # neither pivotal.pem nor private-chef-secrets.json exists, or that both exist.
    validate_sane_state

    # The remaining validation tests apply only if we haven't completed
    # a successful reconfigure
    return unless first_run?

    # At this time, these data-related complications are most helpful
    # if we're configured to use an external DB.  It will be possible
    # to support a managed DB, but will require further consideration -
    # because these checks run before any config recipes,
    # the database server itself won't necessarily be available.
    return unless PrivateChef['postgresql']['external']

    if bypass_bootstrap?
      unless pivotal_user_exists?
        # Since we're not running bootstrap, the pivotal user must have
        # been put there by someone else.
        fail_with err_BOOT011_pivotal_user_does_not_exist
      end
    else
      if pivotal_user_exists?
        # If we're running bootstrap, we need to make sure that nobody else
        # has created the pivotal user.
        fail_with err_BOOT010_pivotal_user_exists
      end
    end

    # TODO:
    # If the pivotal key file exists locally:
    #  - fetch pivotal public key(s) from opscode_chef keys view + users
    #  - extract public key from local pivotal.pem
    #   * If we fail with an auth error at this time, it means that either the
    #     username/password for erchef are not set up (in which case db/schema bootstrap has
    #     not been run and we are working from an outdated secrets file), or we
    #     don't have the correct credentials due to an old secrets file.
    #     This should raise its own error.
    #
    #  - validate that the public key matches any of the returned
    #    pivotal user keys
    #  - if it doesn't:
    #    fail_with err_BOOT008_pivotal_public_key_mismatch
    #  - if it does but the matched key is expired:
    #    fail_with err_BOOT008_pivotal_key_expired
    #  If the pivotal key file does not exist locally, but the secrets file does exist,
    #  it was not copied over from the machine that performed the bootstrap.
  end

  # In order to support a stateless standalone that connects to all-external
  # backend components, allow data bootstrapping to be bypassed when
  # no chef-server-running.json is present but a secrets file is present.
  def bypass_bootstrap?
    first_run? && all_creds_exist? && PrivateChef["postgresql"]["external"]
  end

  private

  def all_creds_exist?
    pivotal_key_exists? && secrets_exists?
  end

  def pivotal_key_exists?
    File.exist? "/etc/opscode/pivotal.pem"
  end

  def validate_sane_state
    if pivotal_key_exists?
      unless secrets_exists?
        fail_with err_BOOT006_pivotal_with_no_secrets
      end
    else
      if secrets_exists?
        fail_with err_BOOT007_secrets_with_no_pivotal
      end
    end
  end

  def pivotal_user_exists?
    connect_as(:superuser, 'silent' => true) do |conn|
      return false unless named_db_exists?(conn, 'opscode_chef')
    end
    connect_as(:superuser, 'silent' => true,'db_name' => 'opscode_chef') do |conn|
      begin
        # The opscode_chef database may not exist - so we'll explicitly
        # connect to it after obtaining a known-cgood connection to the
        # default DB 'template1'
        r = conn.exec("select count(*) result from  users where username = 'pivotal'")
        return r[0]['result'] == '1'
      rescue
        # Since we already have the connection, any error is going to
        # indicate that the table/schema is not yet created - and if that's
        # not created, then the user is not created.
        false
      end
    end
  end


  #TODO:
  #These error messages and condition will need further refinement, details, and validation.
  def err_BOOT001_missing_pivotal_key
<<EOM
BOOT001: Your configuration indicates that you are starting this node
         as part of a cluster, but the required file
         /etc/opscode/pivotal.pem is missing.

         Pending: remediation walkthrough.
EOM
  end

  def err_BOOT002_pivotal_key_exists
<<EOM
BOOT002: Your configuration indicates that you are running an initial reconfigure
         to bring your Chef Server online, but the file /etc/opscode/pivotal.pem
         already exists.

         Pending: remediation walkthrough.
EOM
  end

  def err_BOOT003_missing_pivotal_user
<<EOM
BOOT003: Your configuration indicates that you are starting this node
         as part of a cluster and that initial reconfigure has been performed
         on another node, but the pivotal user does not exist.

         Pending: remediation  walkthrough
EOM
  end

  def err_BOOT004_pivotal_user_exists
<<EOM
BOOT004: You're attempting to run an initial reconfigure, but I see that
         the user 'pivotal' already exists.

         Pending: remediation  walkthrough
EOM
  end

  def err_BOOT005_missing_pivotal_user
<<EOM
BOOT005: Your configuration indicates that you may be starting this node
         as part of a cluster.  However, the superuser `pivotal` does not exist
         within Chef Server.

         Pending: remediation  walkthrough
EOM
  end

  def err_BOOT006_pivotal_with_no_secrets
<<EOM
BOOT006: The pivotal user key is present (/etc/opscode/pivotal.pem)
         but the file /etc/opscode/private-chef-secrets.json is missing.

         Ensure that pivotal.pem is copied over into /etc/opscode from the
         first Chef Server node that you brought online, then run
         'chef-server-ctl reconfigure' again.

EOM

  end

  def err_BOOT007_secrets_with_no_pivotal
<<EOM
BOOT007: The secrets file (/etc/opscode/private-chef-secrets.json) is present
         but the file /etc/opscode/pivotal.pem is missing.

         Ensure that private-chef-secrets.json is copied into /etc/opscode from the
         first Chef Server node that you brought online, then run
         'chef-server-ctl reconfigure' again.
EOM
  end

  def err_BOOT008_pivotal_public_key_mismatch
<<EOM
BOOT008: The pivotal key file /etc/opscode/pivotal.pem exists, but its public key
         does not match the key for the pivotal user in chef-server.

         Critical maintenance operations cannot be performed.

         Pending: remediation walkthrough.
EOM
  end

  def err_BOOT009_pivotal_key_expired
<<EOM
BOOT009: The pivotal superuser account key is expired.

         Critical maintenance operations cannot be performed.

         Pending: remediation walkthrough.
EOM
  end

  def err_BOOT010_pivotal_user_exists
<<EOM
BOOT010: Unable to configure this node because the superuser 'pivotal'
         already exists in the database.

         Please ensure that you have copied the files 'pivotal.pem' and
         'private-chef-secrets.json' from the node to be reconfigured
         into '/etc/opscode/' on this node before attempting to run
         reconfigure again.
EOM
  end

  def err_BOOT011_pivotal_user_does_not_exist
<<EOM
BOOT011: Unable to configure this node because the superuser 'pivotal'
         does not exist in the database.

         If this is the first node you're attempting to reconfigure,
         please remove the files 'pivotal.pem' and 'private-chef-secrets.json'
         from '/etc/opscode' before attempting to run reconfigure again.
EOM
  end
end
