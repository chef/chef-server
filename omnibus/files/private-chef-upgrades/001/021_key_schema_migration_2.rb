define_upgrade do
  if Partybus.config.bootstrap_server
    must_be_data_master
    run_sqitch("@2.9.0", "@1.0.4")
  end
end
