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
  def initialize(node)
    @helper = OmnibusHelper.new(node)
    @node = node
    @previous_run = node['previous_run']
    def fail_with error
      raise PreflightValidationFailed, error
    end
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
