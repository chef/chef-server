define_upgrade do
  if Partybus.config.bootstrap_server
    must_be_data_master
    # Make sure API is down
    stop_services(["nginx", "opscode-erchef"])

    start_services(["oc_bifrost", "postgresql"])
    force_restart_service("opscode-chef-mover")
    log "Creating public_key_read_access group for each existing organization"

    run_command("/opt/opscode/embedded/bin/escript " +
                "/opt/opscode/embedded/service/opscode-chef-mover/scripts/migrate " +
                "mover_actor_keys_access_group_callback " +
                "normal " +
                "mover_transient_queue_batch_migrator")

    stop_services(["opscode-chef-mover"])
  end
end

define_check do
  # If there is at least one group with public_key_read_access group,
  # assume the migration has run. This might not be true, but if there are
  # no existing read_access_groups, we believe the migration should be
  # safe to rerun.
  #
  # We can erroneously mark this migration as "run" in the following cases:
  #
  # (1) The user has created a group called public_read_access
  #     unrelated to this migration.
  #
  # (2) If the user upgraded, failed to apply this upgrade, but then
  #     somehow was able to create *new* organization using the
  #     newer version of erchef.
  #
  # We can erroneously mark this migration as "not run" in the following cases:
  #
  # (1) The migration ran but this installation has no organizations.
  #     Re-running the migration will have no ill effect as it
  #     operates on a per-organization basis.
  #
  # (2) The migration ran but then the user deleted all of the
  #     public_read_access groups. Re-running the migration should not
  #     cause an error but the user might be surprised to find those
  #     groups re-appear.  We think this case is rare.
  #
  begin
    c = pg_conn_for('oc_erchef')
    r = c.exec("SELECT COUNT(*) FROM groups WHERE name = 'public_key_read_access'")
    r.first['count'].to_i > 0
  ensure
    c.close if c
  end
end
