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

require "pg"

class PostgresqlPreflightValidator < PreflightValidator
  attr_reader :cs_pg_attr, :node_pg_attr

  def initialize(node)
    super

    # Note that PrivateChef['postgresql'] will currently contain
    # ONLY the settings specified in chef-server.rb plus some
    # attributes set on initialization of the class.
    @cs_pg_attr = PrivateChef['postgresql']

    # Represents the recipe defaults
    @node_pg_attr = node['private_chef']['postgresql']
  end


  def run!
    #
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


  def external_postgres_config_validation
    # For now we're only performing these checks for external pgsql mode:
    return unless cs_pg_attr['external']
    # These values must be explictly set in chef-server.rb:
    fail_with err_CSPG004_missing_external_host unless cs_pg_attr.has_key? 'vip'
    fail_with err_CSPG002_missing_superuser_id unless cs_pg_attr.has_key? 'db_superuser'
    fail_with err_CSPG003_missing_superuser_password unless cs_pg_attr.has_key? 'db_superuser_password'
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
      if cs_pg_attr.has_key? 'external' && (cs_pg_attr['external'] != previous_run['postgresql']['external'])
        fail_with err_CSPG001_cannot_change_external_flag
      end
    else
      return
    end
  end

  def connectivity_validation
    # all nodes are expected to be able to reach the database node
    # and connect to it - let's make a connection intended to fail
    # just to verify connectivity to the service.
    begin
      connect_as(:invalid_user)
    rescue ::PG::ConnectionBad => e
      # A bit messy but PG gem does not expose error codes to us:
      case e.message
      when /.*Connection refused.*/
        fail_with err_CSPG010_postgres_not_available
      when /.*password authentication failed.*/
        # This is what we want to see.
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
      connect_as(:superuser) do |connection|
        backend_verify_database_access(connection)
        backend_verify_postgres_version(connection)
        %w{bifrost opscode_chef oc_id}.each {|db| backend_verify_named_db_not_present(connection, db) }
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

private
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
    # NOTE: our current entry in version-manifest.json is 'postgres92',effectively hardcoding the version.
    # TODO: We will want to ensure that current postgres is always labeled as 'postgres' which will have migration
    #       impacts, OR we create a placeholder 'postgres-version' component that exists only to capture the
    #       actual current version.
    manifest = JSON.parse(File.read("/opt/opscode/version-manifest.json"))
    required_major, required_minor = manifest['software']['postgresql92']['locked_version'].split(".")
    # that- note that our current key in chefwe want to pull in our requirement from our build-time configuration
    # and not hard-code it here.
    unless major == required_major and minor == required_minor
      fail_with err_CSPG014_bad_postgres_version(v)
    end
  end

  def backend_verify_named_db_not_present(connection, name)
    # This test is only valid on our initial run - bootstrap itself is not a sufficent check,
    # because we may have partially bootstrapped.
    return if previous_run
    r = connection.exec("SELECT count(*) AS result FROM pg_database WHERE datname='name'")
    Chef::Log.fatal(r[0])
    fail_with err_CSPG016_database_exists(name) unless r[0]['result'] == '0'
  end

  def connect_as(type)
    port = cs_pg_attr.has_key?('port') ? cs_pg_attr['port'] : node_pg_attr['port']
    host = cs_pg_attr['vip']
    if type == :invalid_user
      user = 'chef_server_conn_test'
      password = 'invalid'
    else
      user = cs_pg_attr['db_superuser']
      password = cs_pg_attr['db_superuser_password']
    end
    # We just want this to throw an exception or not - caller knows what to do with it.
    EcPostgres.with_connection(node, 'template1', { 'db_superuser' => user,
                                                    'db_superuser_password' => password,
                                                    'vip' => host,
                                                    'port' => port}) do |conn|
       if block_given?
         yield(conn)
       end
    end
  end

  def err_CSPG001_cannot_change_external_flag
<<EOM
CSPG001: The value of postgresql['external'] must be set prior to the initial
         run of chef-server-ctl reconfigure and cannot be changed.

         See https://docs.chef.io/TODO-external-pg-upgrade-existing-installation
         for more information on how you can transition an existing chef-server
         to a new instance configured for an external database and vice-versa.
EOM
  end


  def err_CSPG002_missing_superuser_id
<<EOM
CSPG002: You have not set a database superuser name under
         "postgresql['db_superuser'] in
         chef-server.rb.  This is required for external database support - please set
         it now and then re-run 'chef-server-ctl reconfigure'.

         See https://docs.chef.io/TODO-external-pg-chef-server-configuration
         for more information.
EOM
  end

  def err_CSPG003_missing_superuser_password
<<EOM
CSPG003: You have not set a database superuser password under
         "postgresql['db_superuser_password']" in chef-server.rb.  This is
         required for external database support - please set it now and then
         re-run 'chef-server-ctl reconfigure'.

         See documentation at https://docs.chef.io/TODO-external-pg-chef-server-configuration
         for more information.
EOM
  end

  def err_CSPG004_missing_external_host
<<EOM
CSPG004: Because postgresql['external'] is set to true, you must also set
         postgresql['vip'] to the host or IP of an external postgres database
         in chef-server.rb.

         See documentation at https://docs.chef.io/TODO-external-pg-chef-server-configuration
         for more information.
EOM
  end

  def err_CSPG010_postgres_not_available
<<EOM
CSPG010: I cannot make a connection to the host #{cs_pg_attr['vip']}.  Please
         verify that the host is online and reachable from this node, and that
         you have configured postgresql['port'] if it's not the standard
         port 5432, then run 'chef-server-ctl reconfigure' again.

         See https://docs.chef.io/TODO-external-pg-config#networking
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

         See https://docs.chef.io/TODO-external-pg-chef-server-configuration
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

         See https://docs.chef.io/TODO-external-pg-config#pg_hba
         for more information.
EOM
  end
  def err_CSPG013_not_superuser
<<EOM
CSPG013: The superuser account '#{cs_pg_attr['db_superuser']}' does not have
         superuser access to the to the database specified.  At minimum, this
         user must be granted CREATE DATABASE and CREATE ROLE privileges.

         See https://docs.chef.io/TODO-external-pg-config#access_levels
         for more information.
EOM
  end

  def err_CSPG014_bad_postgres_version(ver)
<<EOM
CSPG014: Chef Server currently requires PostgreSQL version 9.2 or greater.
         The database you have provided is running version #{ver}.

         See https://docs.chef.io/TODO-external-pg-config#requirements
         for more information.
EOM
  end

  def err_CSPG015_template1_db_required
<<EOM
CSPG015: The database server you provided does not have the default database
         template1 available.  Please create the template1 database before
         proceeding.

         See https://docs.chef.io/TODO-external-pg-config#requirements
         for more information.
EOM
  end


  def err_CSPG016_database_exists(dbname)
<<EOM
CSPG016: The Chef Server database named `#{dbname}` already exists on the
         PostgreSQL server. Please remove it before proceeding.

         See https://docs.chef.io/TODO-external-pg-config#requirements
         for more information.
EOM
  end

end

