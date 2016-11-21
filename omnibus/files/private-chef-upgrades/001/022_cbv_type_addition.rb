define_upgrade do
  if Partybus.config.bootstrap_server
    must_be_data_master
    # Performance improvements for cookbook fetching.
    run_sqitch('@cbv-type', 'oc_erchef')
  end
end
