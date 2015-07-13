class PostgresqlPreflightValidator
  attr_reader :node, :nodehelp, :pg_attr


  def initialize(node)
    @node = node
    @pg_attr = PrivateChef['postgresql']
  end


  def run!
    verify_unchanged_external_flag
    common_validation
    external_postgres_validation
    backend_configuration_validation
  end

  def backend_configuration_validation
      verify_unchanged_external_flag
  end
  def common_validation
    # all nodes are expected to be able to reach the database node
    # and connect to it - let's make a connection intended to fail
    # just to verify connectivity to the service.
  end
  def external_postgres_validation
    return unless @pg_attr['external'] == true
    # All nodes need to know what host to connect to
    raise     @pg_attr['vip']

    # The host must be reachable from here.

  end

  # We do not support changing from managed to external DB or vice-versa, so the
  # 'external' flag may not be changed in any scenario - it must be set from the first run
  # of chef-server-ctl reconfigure.
  def verify_unchanged_external_flag
    return unless OmnibusHelper.has_been_bootstrapped?
    if node['previous_run'] and
        pg_attr['external'] != node['previous_run']['postgresql']['external']
      raise PreflightChecks::ValidationFailed, ERR_CANNOT_CHANGE_EXTERNAL_FLAG
    end
  end




  ERR_CANNOT_CHANGE_EXTERNAL_FLAG = <<EOM
CSPG001: The value of postgresql['external'] must be set prior to the initial run of
chef-server-ctl reconfigure and cannot be changed.

See documentation at https://docs.chef.io/TODO-upgrading-existing-installation
EOM
end
