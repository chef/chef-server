define_upgrade do
  upgrade_schema_to 23
  run_command "cd /opt/opscode/embedded/service/opscode-reporting/db && sudo -u opscode-pgsql PATH=/opt/opscode/embedded/bin:$PATH /opt/opscode/embedded/bin/bundle exec /opt/opscode/embedded/bin/rake pg:migrate"
  restart_service "opscode-erchef"
  restart_service "opscode-reporting"
end
