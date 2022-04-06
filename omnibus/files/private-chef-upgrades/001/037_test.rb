define_upgrade do
  run_sqitch('test_add_table', 'oc_erchef')
end

define_check do
  check_sqitch('test_add_table', 'oc_erchef')
end