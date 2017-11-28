define_upgrade do
  if Partybus.config.bootstrap_server
    must_be_data_master
    # Add policyfile fields to node table
    run_sqitch('@node-policyfile-fields', 'oc_erchef')
  end
end

define_check do
  check_sqitch('@node-policyfile-fields', 'oc_erchef')
end
