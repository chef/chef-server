define_upgrade do
  start_service('postgresql')
  upgrade_schema_to 33
  stop_service('postgresql')
  restart_service "opscode-erchef"
end
