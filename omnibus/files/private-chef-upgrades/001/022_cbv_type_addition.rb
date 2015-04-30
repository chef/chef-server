define_upgrade do
  if Partybus.config.bootstrap_server
    must_be_data_master
    run_sqitch("@cbv-type", "@1.0.4")
  end
end
