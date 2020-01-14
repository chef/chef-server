
define_upgrade do
  upgrade_schema_to 32

  if Partybus.config.bootstrap_server

    must_be_data_master

    # Only starting opscode-account and couchdb so that we can
    # delete pre-created orgs. Those two are stopped right after
    # pre-created orgs are deleted.
    start_services(['opscode-account', 'couchdb'])

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

    down_services = ['nginx',
                     'couchdb',
                     'opscode-org-creator',
                     'bookshelf',
                     'oc_bifrost',
                     'opscode-account',
                     'opscode-certificate',
                     'opscode-erchef',
                     'opscode-expander',
                     'opscode-expander-reindexer',
                     'opscode-solr4']

    # Shut down everything but couch & postgres
    down_services.each{|s| run_command("private-chef-ctl stop #{s}")}

    # Remove any mover log files from a previous run, if they exist.
    # The log message parser requires a "clean slate".
    mover_log_file_glob    = "/var/log/opscode/opscode-chef-mover/console.log*"
    parsed_log_output = "/var/log/opscode/opscode-chef-mover/parsed_console.log"
    run_command("rm -f #{mover_log_file_glob}")
    run_command("rm -f #{parsed_log_output}")

    # Restart chef-mover for the duration of the migration
    run_command("private-chef-ctl restart opscode-chef-mover")
    sleep(60)

    # Perform the actual migration
    run_command("/opt/opscode/embedded/bin/escript /opt/opscode/embedded/service/opscode-chef-mover/scripts/migrate")

    run_command("./check_logs.rb #{parsed_log_output} #{mover_log_file_glob}",
                :cwd => "/opt/opscode/embedded/service/opscode-chef-mover/scripts")

    # We don't need chef-mover anymore
    run_command("private-chef-ctl stop opscode-chef-mover")
  end

end
