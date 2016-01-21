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

class PreflightValidationFailed < StandardError

end

class PreflightValidator
  attr_reader :node, :previous_run, :helper
  attr_reader :cs_pg_attr, :node_pg_attr
  def initialize(node)
    @helper = OmnibusHelper.new(node)
    @node = node
    @previous_run = node['previous_run']

    # Note that PrivateChef['postgresql'] will currently contain
    # ONLY the settings specified in chef-server.rb plus some
    # attributes set on initialization of the class.
    @cs_pg_attr = PrivateChef['postgresql']

    # Represents the recipe defaults
    @node_pg_attr = node['private_chef']['postgresql']

  end

  def fail_with error
    raise PreflightValidationFailed, error
  end

  def first_run?
    previous_run.nil?
  end

  def secrets_exists?
    PrivateChef.existing_secrets.length > 0
  end

  def backend?
    #
    # When these preflight checks are run, the chef-server.rb has been ingested into
    # PrivateChef but has not been merged into the node. This means for accurate results, we'll
    # need to provide an assembled node object that contains the configuration that
    # the backend check needs.
    faux_node = { 'enterprise' => node['enterprise'],
                  node['enterprise']['name'] =>  PrivateChef }
    EnterpriseChef::Helpers.backend? faux_node
  end

  # Postgresql connectivity and validation functions.
  def named_db_exists?(connection, name)
    connection.exec("SELECT count(*) AS result FROM pg_database WHERE datname='#{name}'")[0]['result'] == '1'
  end

  def named_role_exists?(connection, username)
    # If a record exists, the role exists:
    connection.exec("select usesuper from pg_catalog.pg_user where usename = '#{username}'").ntuples > 0
  end

  def connect_as(type, db_name = 'template1')
    require "pg" # Make the PG constants available to the caller
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
    EcPostgres.with_connection(node, db_name, { 'db_superuser' => user,
                                                'db_superuser_password' => password,
                                                'vip' => host,
                                                'port' => port,
                                                # By default, pass exceptions up without retrying
                                                # We have a few scenarios in which we
                                                # expect errors, and don't want to delay the bootstrap
                                                # for unnecessary retries.
                                                'retries' => 0 }) do |conn|
       if block_given?
         yield(conn)
       end
    end
  end
  # Helper function that let get the correct top-level value for a service
  # node entry, given that we haven't yet merged PrivateChef
  # into the node.
  def service_key_value(service_name, key)
    # Ugh: config key munging pain redux
    alt_service_name = (service_name == 'opscode-erchef' ? 'opscode_erchef' : service_name)
    if PrivateChef.has_key?(alt_service_name)
      if PrivateChef[alt_service_name].has_key?(key)
        return PrivateChef[alt_service_name][key]
      end
    end
    return node['private_chef'][service_name][key]
  end
end

class PreflightChecks
  attr_reader :node
  def initialize(node)
    @node = node
  end


  # Run our validators to ensure we're in a good state to perform a reconfigure/chef client run.
  # Stop the run immediately if a validation failure occurs, bypassing normal error handlers
  # so we can output the error message without a stack trace to muddy things.
  #
  # Validators are expected to be run after chef-server.rb entries are loaded but before
  # they're ingested into the node, and before any defaults are configured via libraries/private_chef.rb
  # This allows us to check the values that are explicitly configured independently of
  # the defaults set in the recipe.
  def run!
    begin
      BootstrapPreflightValidator.new(node).run!
      PostgresqlPreflightValidator.new(node).run!
      SolrPreflightValidator.new(node).run!
    rescue PreflightValidationFailed => e
      # use of exit! prevents exit handlers from running, ensuring the last thing
      # the customer sees is the descriptive error we've provided.
      Chef::Log.fatal("\n\n#{LINE_SEP}\n#{e.message}#{LINE_SEP}")
      exit! 128
    end
  end
  LINE_SEP = "-----------------------------------------------------------------------"
end
