define_upgrade do

  if Partybus.config.bootstrap_server
    must_be_data_master

    # schema updates include adding org association/user tables
    # and adding the OSC password hash types to the password_hash_type_enum
    run_sqitch('@2.4.0', 'oc_erchef')
  end
end

define_check do
  check_sqitch('@2.4.0', 'oc_erchef')
end
