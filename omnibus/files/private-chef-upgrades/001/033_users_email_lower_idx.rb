define_upgrade do

  if Partybus.config.bootstrap_server
    must_be_data_master

    # schema update for adding a functional index for index-supported queries
    # using `WHERE lower(email) = lower("USER@FOO.com")`.
    run_sqitch('@users_email_functional_index', 'oc_erchef')
  end
end

define_check do
  check_sqitch('@users_email_functional_index', 'oc_erchef')
end
