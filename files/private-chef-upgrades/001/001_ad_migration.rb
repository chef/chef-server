define_upgrade do
  upgrade_schema_to 11
  restart "opscode-account"
  restart "opscode-webui"
end
