define_upgrade do
  upgrade_schema_to 23
  restart_service "opscode-erchef"
end
