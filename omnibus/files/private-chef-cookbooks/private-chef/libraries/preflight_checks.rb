class PreflightValidationFailed < Exception

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
    return unless EnterpriseChef::Helpers.backend? faux_node

    return PrivateChef['topology'] == 'standalone' || PrivateChef['role'] == 'backend'
  end
end

class PreflightChecks
  attr_reader :node
  def initialize(node)
    @node = node
  end

  # Any validator is expected to throw a PreflightChecks::ValidationFailed exception if the run
  # must be stopped.

  # Run our validators to ensure we're in a good state to perform a reconfigure/chef client run.
  # Stop the run immediately if a validation failure occurs.
  # Validators are expected to be run after chef-server.rb entries are ingested, but before any
  # defaults are configured via libraries/private_chef.rb
  def run!
    begin
      PostgresqlPreflightValidator.new(node).run!
    rescue PreflightValidationFailed => e
      # use of exit! prevents exit handlers from running, ensuring the last thing
      # the customer sees is the descriptive error we've provided.
      Chef::Log.fatal("\n\n#{LINE_SEP}\n#{e.message}#{LINE_SEP}")
      exit! 128
    end
  end
  LINE_SEP = "-----------------------------------------------------------------------"
end
