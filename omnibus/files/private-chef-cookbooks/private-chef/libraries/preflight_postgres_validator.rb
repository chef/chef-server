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

class PostgresqlPreflightValidator < PreflightValidator

  def initialize(node)
    super

  end

  def run!
    verify_unchanged_external_flag

    # Our additional validations only apply when a database server exists,
    # which - for the time being - means we're only going to run these checks
    # if external DB is configured.
    return unless cs_pg_attr['external']
    external_postgres_config_validation

    # In the future, we should be able to run these
    # on non-backend nodes:
    connectivity_validation
    backend_validation
  end

  def bookshelf_in_sql?
    @cs_pg_attr['bookshelf'] && @cs_pg_attr['bookshelf']['storage_type'].to_s == "sql"
  end

  def databases_to_check
    d = %w{bifrost opscode_chef oc_id}
    d << 'bookshelf' if bookshelf_in_sql?
    d
  end

  def roles_to_check
    d = %w{opscode-erchef oc_id oc_bifrost}
    d << 'bookshelf' if bookshelf_in_sql?
    d
  end

  def external_postgres_config_validation
    # For now we're only performing these checks for external pgsql mode:
    return unless cs_pg_attr['external']
    # These values must be explictly set in chef-server.rb:
    fail_with err_CSPG004_missing_external_host unless cs_pg_attr.has_key? 'vip'
    fail_with err_CSPG002_missing_superuser_id unless cs_pg_attr.has_key? 'db_superuser'
    fail_with err_CSPG003_missing_superuser_password unless has_superuser_password?
  end

  def has_superuser_password?
    PrivateChef.credentials.exist?("postgresql", "db_superuser_password")
  end

  # We do not support changing from managed to external DB or vice-versa, so the
  # 'external' flag may not be changed in any scenario - it must be set from the first run
  # of chef-server-ctl reconfigure.
  def verify_unchanged_external_flag

    # Given that someone may  move a back-end to a new instance and update the
    # front-ends with the shared chef-server.rb, we may reasonably expect to
    # see this value change on the front end - so let's not call that out as an error.

    # This test isn't valid until we're bootstrapped (which could
    # encompass multiple runs in case of error). Before bootstrap,
    # the default value of 'false' will be in place, and having a
    # differing value is valid.
    if OmnibusHelper.has_been_bootstrapped? && backend?  && previous_run
      if (cs_pg_attr.has_key? 'external') && (cs_pg_attr['external'] != previous_run['postgresql']['external'])
        fail_with err_CSPG001_cannot_change_external_flag
      end
    else
      return
    end
  end

  def connectivity_validation
    # all nodes are expected to be able to reach the database node
    # and connect to it - let's make a connection intended to fail
    # with an auth-related error just to verify connectivity to the service.
    begin
      connect_as(:invalid_user, 'silent' => true, 'retries' => 0)
    rescue ::PG::ConnectionBad => e
      # A bit messy but PG gem does not expose error codes to us:
      case e.message
      when /.*Connection refused.*/
        fail_with err_CSPG010_postgres_not_available
      when /.*password authentication failed.*/
        # This is what we want to see.
      when /role .* does not exist/
        # This indicates we were successfully able to connect to Postgres
        # AND we authenticated! This is likely because the pg_hba is set
        # to trust our connection. Such an example would be configuring
        # Chef Server to use an "external" Postgres, such as Delivery's,
        # when running on the same host.
      when /.*no pg_hba.conf entry.*/
        # This is also possible, depending on if they've set up pg_hba
        # by host or user or both. This is OK, since it confirms that we're
        # able to connect to the postgres instance.
      else
        # This shouldn't be possible, but we still don't want to dump an
        # unhelpful stack trace on the screen. Let's at least catch it and
        # fail with a meaningful error.
        fail_with "CSPG999: #{e.message}"
      end
    end
  end

  def backend_validation
    begin
      connect_as(:superuser, 'silent' => true) do |connection|
        backend_verify_database_access(connection)
        backend_verify_postgres_version(connection)
        # The database should only exist if we haven't bootstrapped the previous
        # run, or secrets haven't been copied from previous node.
        if first_run?
          if secrets_exists?
            return
          end
        else
          return
        end
        databases_to_check.each {|db| backend_verify_named_db_not_present(connection, db)}
        backend_verify_cs_roles_not_present(connection)
      end
    rescue ::PG::InsufficientPrivilege => e
      fail_with err_CSPG013_not_superuser
    rescue ::PG::ConnectionBad => e
      case e.message
      when /.*database 'template1', does not exist.*/
        fail_with err_CSPG015_template1_db_required
      when /.*password authentication failed.*/
        fail_with err_CSPG011_invalid_superuser_account
      when /.*no pg_hba.conf entry.*/
        fail_with err_CSPG012_invalid_pg_hba
      else
        # As above - we shouldn't hit this path, but if we do let's at least
        # show a meaningful error instead of a stack trace.
        fail_with "CSPG999: #{e.message}"
      end
    end
  end

  def backend_verify_database_access(connection)
    # Make sure we have the access we need.
    # Note on not escaping username: we already connected with the same attribute,
    # so it's safe at this point.
    r = connection.exec("SELECT rolsuper, rolcreaterole, rolcreatedb FROM pg_roles WHERE rolname='#{cs_pg_attr['db_superuser']}';")
    # a super user may not have createrole/createdb flags set, so check for both cases
    unless (r[0]['rolsuper'] == 't') or (r[0]['rolcreaterole'] == 't' and r[0]['rolcreatedb'] == 't')
      fail_with err_CSPG013_not_superuser
    end
  end

  def backend_verify_postgres_version(connection)
    # Make sure the server is a supported version.
    r = connection.exec("SHOW server_version;")
    v = r[0]['server_version']
    major, minor = v.split(".")
    # Load up our required major/minor:
    # NOTE: our current entry in version-manifest.json is 'postgres92',effectively hardcoding the version. Necessary
    # change is captured here: https://github.com/chef/chef-server/issues/441
    manifest = JSON.parse(File.read("/opt/opscode/version-manifest.json"))
    required_major, required_minor = manifest['software']['postgresql92']['locked_version'].split(".")

    # Note that we're looking for the same major, and using our minor as the minimum version
    # This provides compatibility with external databases that use 9.3+ before we officially upgrade to it.
    unless major == required_major and minor >= required_minor
      fail_with err_CSPG014_bad_postgres_version(v)
    end
  end

  # Throws CSPG017 if any of the reserved usernames we
  # need to create are already in the datbase.
  def backend_verify_cs_roles_not_present(connection)
    roles_to_check.each do |service|
      %w{sql_user sql_ro_user}.each do |key|
        username = service_key_value(service, key)
        fail_with err_CSPG017_role_exists(username) if named_role_exists?(connection, username)
      end
    end
  end

  def backend_verify_named_db_not_present(connection, name)
    fail_with err_CSPG016_database_exists(name) if named_db_exists?(connection, name)
  end

private

  def err_CSPG001_cannot_change_external_flag
<<EOM
CSPG001: The value of postgresql['external'] must be set prior to the initial
         run of chef-server-ctl reconfigure and cannot be changed.

         See https://docs.chef.io/error_messages.html#cspg001-changed-setting
         for more information on how you can transition an existing chef-server
         to a new instance configured for an external database and vice-versa.
EOM
  end

  def err_CSPG002_missing_superuser_id
<<EOM
CSPG002: You have not set a database superuser name under
         "postgresql['db_superuser']" in chef-server.rb.  This is required
         for external database support - please set it now and
         then re-run 'chef-server-ctl reconfigure'.

         See https://docs.chef.io/server_components.html#postgresql-settings
         for more information.
EOM
  end

  def err_CSPG003_missing_superuser_password
<<EOM
CSPG003: You have not set a database superuser password using
         chef-server-ctl set-db-superuser-password. This is required
         for external database support - please run this now, then
         re-run 'chef-server-ctl reconfigure'.

         See https://docs.chef.io/server_components.html#postgresql-settings
         for more information.
EOM
  end

  def err_CSPG004_missing_external_host
<<EOM
CSPG004: Because postgresql['external'] is set to true, you must also set
         postgresql['vip'] to the host or IP of an external postgres database
         in chef-server.rb.

         See https://docs.chef.io/server_components.html#postgresql-settings
         for more information.
EOM
  end

  def err_CSPG010_postgres_not_available
<<EOM
CSPG010: I cannot make a connection to the host #{cs_pg_attr['vip']}.  Please
         verify that the host is online and reachable from this node, and that
         you have configured postgresql['port'] if it's not the standard
         port 5432, then run 'chef-server-ctl reconfigure' again.

         See https://docs.chef.io/error_messages.html#cspg010-cannot-connect
         for more information about postgresql networking requirements.
EOM
  end

  def err_CSPG011_invalid_superuser_account
<<EOM
CSPG011: I could not authenticate to #{cs_pg_attr['vip']} as
         #{cs_pg_attr['db_superuser']} using the password provided.
         Please make sure that the the password you provided in
         chef-server.rb under "postgresql['db_superuser_password'] is correct
         for this user.

         See https://docs.chef.io/error_messages.html#cspg011-cannot-authenticate
         for more information.
EOM
  end

  def err_CSPG012_invalid_pg_hba
<<EOM
CSPG012: There is a missing or incorrect pg_hba.conf entry for the
         user '#{cs_pg_attr['db_superuser']}' and/or this originating host.
         Please ensure that pg_hba.conf entries exist to allow the superuser
         account to connect from the Chef Server backend nodes, and to
         allow the application accounts to connect from all Chef Server
         nodes.

         See https://docs.chef.io/error_messages.html#cspg012-incorrect-rules
         for more information.
EOM
  end
  def err_CSPG013_not_superuser
<<EOM
CSPG013: The superuser account '#{cs_pg_attr['db_superuser']}' does not have
         superuser access to the to the database specified.  At minimum, this
         user must be granted CREATE DATABASE and CREATE ROLE privileges.

         See https://docs.chef.io/error_messages.html#cspg013-incorrect-permissions
         for more information.
EOM
  end

  def err_CSPG014_bad_postgres_version(ver)
<<EOM
CSPG014: Chef Server currently requires PostgreSQL version 9.2 or greater.
         The database you have provided is running version #{ver}.

         See https://docs.chef.io/error_messages.html#cspg014-incorrect-version
         for more information.
EOM
  end

  def err_CSPG015_template1_db_required
<<EOM
CSPG015: The database server you provided does not have the default database
         template1 available.  Please create the template1 database before
         proceeding.

         See https://docs.chef.io/error_messages.html#cspg015-missing-database
         for more information.
EOM
  end

  def err_CSPG016_database_exists(dbname)
<<EOM
CSPG016: The Chef Server database named '#{dbname}' already exists on the
         PostgreSQL server. Please remove it before proceeding.

         See https://docs.chef.io/error_messages.html#cspg016-database-exists
         for more information.
EOM
  end

  def err_CSPG017_role_exists(username)
<<EOM
CSPG017: The Chef Server database role/user named '#{username}' already exists
         on the PostgreSQL server. If possible, please remove this user
         via 'DROP ROLE "#{username}"' before proceeding, or reference the
         troubleshooting link below for information about configuring
         Chef Server to use an alternative user name.

         See https://docs.chef.io/error_messages.html#cspg017-user-exists
         for more information.
EOM

  end
end
