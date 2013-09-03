
define_upgrade do
  upgrade_schema_to 32
  restart_service "opscode-account"
  restart_service "opscode-erchef"

  # Need to remove any existing pre-created orgs, since chef-mover
  # doesn't migrate them.  Otherwise, the next handful of orgs that
  # got created would have bogus _default environments.

  # CWD is needed for Bundler to find to correct Gemfile for Orgmapper
  # (Otherwise, we end up getting the one from Partybus).  RUBYOPT is
  # needed for the script to find Bundler in the first place.
  #
  #      ;_;
  #
  # Technically, we could remove CWD and add a BUNDLER_GEMFILE to the
  # environment, but that's just damn ugly.
  run_command("/opt/opscode/embedded/bin/ruby scripts/delete-pre-created-orgs.rb /etc/opscode/orgmapper.conf all",
              :cwd => "/opt/opscode/embedded/service/opscode-platform-debug/orgmapper",
              :env => {"RUBYOPT" => "-I/opt/opscode/embedded/lib/ruby/gems/1.9.1/gems/bundler-1.1.5/lib"})
  restart_service "opscode-org-creator"


  # Perform the actual migration

  mover_log_file_glob    = "/var/log/opscode/opscode-chef-mover/console.log*"
  parsed_log_output = "/var/log/opscode/opscode-chef-mover/parsed_console.log"

  # Remove any mover log files from a previous run, if they exist.
  # The log message parser requires a "clean slate".
  run_command("rm -f #{mover_log_file_glob}")
  run_command("rm -f #{parsed_log_output}")

  run_command("/opt/opscode/embedded/bin/escript /opt/opscode/embedded/service/opscode-chef-mover/scripts/migrate")

  run_command("./check_logs.rb #{parsed_log_output} #{mover_log_file_glob}",
              :cwd => "/opt/opscode/embedded/service/opscode-chef-mover/scripts")

  restart_service "opscode-account"
  restart_service "opscode-erchef"

  # Restarting nginx at this point appears to be necessary; otherwise,
  # creation of new environments seems to not be going through erchef
  # for some reason.
  restart_service "nginx"

end
