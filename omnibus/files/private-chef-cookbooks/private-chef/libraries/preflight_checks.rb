class PreflightChecks
  attr_reader :node
  def initialize(node)
    @node = node
  end

  # Any validator is expected to throw a PreflightChecks::ValidationFailed exception if the run
  # must be stopped.
  class ValidationFailed < Exception

  end

  # Run our validators to ensure we're in a good state to perform a reconfigure/chef client run.
  # Stop the run immediately if a validation failure occurs.
  # We will do so in a way that prevents the default at_exit handlers from being invoked -
  # which means that the error text is immediately visible to the user, without forcing
  # them to hunt for it in backscroll.
  #
  # Validations are expected to be run after chef-server.rb entries are ingested, but before any
  # defaults are configured via libraries/private_chef.rb
  def run!
    begin
      PostgresqlPreflightValidator.new(node).run!
    rescue PreflightChecks::ValidationFailed => e
      error_exit(e.message)
    end
  end

  # Forcibly exit with the error message provided.
  def error_exit(error)
    puts ""
    puts LINE_SEP
    puts ""
    puts error
    puts ""
    puts LINE_SEP
    exit!
  end

  LINE_SEP = "-----------------------------------------------------------------------"
end
