define_upgrade do

  if Partybus.config.bootstrap_server

    must_be_data_master

    # run 2.2.4 migration which includes schema upgrade for migration state
    run_sqitch("@2.2.4", "@1.0.4")
  end
end
