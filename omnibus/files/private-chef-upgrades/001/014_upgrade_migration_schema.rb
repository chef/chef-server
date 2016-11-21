define_upgrade do

  if Partybus.config.bootstrap_server
    must_be_data_master

    # This ensures that sqitch is properly set up, even when upgrading from private chef (which did
    # not use sqitch), as well as ensuring the baseline schema is present for opscode_chef.
    #
    # Note that we only have to apply it in this upgrade, because this is the first sqitch migration
    # that will be run.  If this doesn't need to be run, that tells us they're upgrading from an installation
    # that already has applied it.
    run_sqitch('@1.0.4', 'oc_erchef', path: 'opscode-erchef/schema/baseline')

    # The actual schema change for this release -
    # track more state values in migration_state.
    run_sqitch('@2.2.4', 'oc_erchef')
  end
end
