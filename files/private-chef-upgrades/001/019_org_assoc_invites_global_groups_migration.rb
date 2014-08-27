define_upgrade do
  if Partybus.config.bootstrap_server

    must_be_data_master

    # Make sure API is down
    stop_services(["nginx", "opscode-erchef"])

    # start postgres, as well as opscode-account and couchdb
    # we can delete pre-created orgs (will shut the latter two
    # down after we delete pre-created orgs).
    start_services(['postgresql', 'opscode-account', 'couchdb'])

    # delete pre-created orgs
    run_command("/opt/opscode/embedded/bin/ruby scripts/delete-pre-created-orgs.rb /etc/opscode/orgmapper.conf all",
                :cwd => "/opt/opscode/embedded/service/opscode-platform-debug/orgmapper",
                :env => {"RUBYOPT" => "-I/opt/opscode/embedded/lib/ruby/gems/1.9.1/gems/bundler-1.1.5/lib"})
    stop_services(['opscode-account', 'couchdb'])

    # clean logs pre-migration
    clean_mover_logs

    force_restart_service("opscode-chef-mover")

    log "Migrating organizations..."
    run_command("/opt/opscode/embedded/bin/escript " \
                "/opt/opscode/embedded/service/opscode-chef-mover/scripts/migrate " \
                "mover_org_migration_callback normal mover_transient_queue_batch_migrator")

    # Currently, mover will crash if you try to run moser_acct_processor:open_account() multiple
    # times in the same console session. To get around this, reboot mover. Time willing, we should
    # track down this issue.
    force_restart_service("opscode-chef-mover")

    log "Migrating organization user associations..."
    run_command("/opt/opscode/embedded/bin/escript " \
                "/opt/opscode/embedded/service/opscode-chef-mover/scripts/migrate " \
                "mover_org_user_association_migration_callback normal mover_transient_queue_batch_migrator")

    force_restart_service("opscode-chef-mover")

    log "Migrating organization association requests..."
    run_command("/opt/opscode/embedded/bin/escript " \
                "/opt/opscode/embedded/service/opscode-chef-mover/scripts/migrate " \
                "mover_org_user_invites_migration_callback normal mover_transient_queue_batch_migrator")

    force_restart_service("opscode-chef-mover")

    log "Migrating global groups..."
    run_command("/opt/opscode/embedded/bin/escript " \
                "/opt/opscode/embedded/service/opscode-chef-mover/scripts/migrate " \
                "mover_global_groups_migration_callback normal mover_transient_queue_batch_migrator")

    # TODO: add log parsing to ensure migration succeeded

    stop_services(["opscode-chef-mover", "postgresql"])

  end
end
