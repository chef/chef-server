define_upgrade do
  upgrade_schema_to 33
  restart_service "opscode-erchef"
end
