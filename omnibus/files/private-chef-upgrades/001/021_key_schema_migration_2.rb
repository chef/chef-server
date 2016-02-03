define_upgrade do
  if Partybus.config.bootstrap_server
    must_be_data_master
    # 1. cookbook artifacts
    # 2. adds last update tracking to keys table.
    run_sqitch('@2.9.0', 'oc_erchef')
  end
end
