define_upgrade do
  upgrade_schema_to 17
  restart_service "opscode-webui"
  restart_service "opscode-account"
  restart_service "opscode-chef"
  restart_service "opscode-erchef"
  restart_service "nginx"
end
