define_upgrade do

  if Partybus.config.bootstrap_server
    must_be_data_master

    # schema update no longer insert public_key in users table.
    # the keys table is the only source of truth
    run_sqitch('@sentinel_public_key_for_users', 'oc_erchef')
  end
end
