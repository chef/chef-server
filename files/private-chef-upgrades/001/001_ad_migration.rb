define_upgrade do
  upgrade_schema_to 11
  restart_service "opscode-account"
end
