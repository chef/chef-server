
define_upgrade do
  upgrade_schema_to 32
  restart_service "opscode-account"
  restart_service "opscode-erchef"
end
