
define_upgrade do
  upgrade_schema_to 32

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

  # Shut down everything but couch & postgres
  run_command("private-chef-ctl stop nginx")
  run_command("private-chef-ctl stop opscode-org-creator")
  run_command("private-chef-ctl stop bookshelf")
  run_command("private-chef-ctl stop oc_bifrost")
  run_command("private-chef-ctl stop opscode-account")
  run_command("private-chef-ctl stop opscode-certificate")
  run_command("private-chef-ctl stop opscode-erchef")
  run_command("private-chef-ctl stop opscode-expander")
  run_command("private-chef-ctl stop opscode-expander-reindexer")
  run_command("private-chef-ctl stop opscode-solr")
  run_command("private-chef-ctl stop opscode-webui")
  run_command("private-chef-ctl stop opscode-rabbitmq")

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

  # Bring everything back up
  run_command("private-chef-ctl start opscode-rabbitmq")
  run_command("private-chef-ctl start opscode-webui")
  run_command("private-chef-ctl start opscode-solr")
  run_command("private-chef-ctl start opscode-expander-reindexer")
  run_command("private-chef-ctl start opscode-expander")
  run_command("private-chef-ctl start opscode-erchef")
  run_command("private-chef-ctl start opscode-certificate")
  run_command("private-chef-ctl start opscode-account")
  run_command("private-chef-ctl start oc_bifrost")
  run_command("private-chef-ctl start bookshelf")
  run_command("private-chef-ctl start opscode-org-creator")
  run_command("private-chef-ctl start nginx")

end
