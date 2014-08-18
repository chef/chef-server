# This is the chef-mover `phase_2_prep_migration` and `phase_2_migration`
# that migrates containers and groups from couchDB to postgreSQL

require 'time'

define_upgrade do
  if Partybus.config.bootstrap_server

    must_be_data_master

    # Make sure API is down
    stop_services(["nginx", "opscode-erchef"])

    # start postgres, as well as opscode-account and couchdb
    # we can delete pre-created orgs (will shut the latter two
    # down after we delete pre-created orgs).
    start_services(['postgresql', 'opscode-account', 'couchdb'])

    run_command("/opt/opscode/embedded/bin/ruby scripts/delete-pre-created-orgs.rb /etc/opscode/orgmapper.conf all",
                :cwd => "/opt/opscode/embedded/service/opscode-platform-debug/orgmapper",
                :env => {"RUBYOPT" => "-I/opt/opscode/embedded/lib/ruby/gems/1.9.1/gems/bundler-1.1.5/lib"})
    stop_services(['opscode-account', 'couchdb'])

    # Move any mover log files from a previous run, if they exist.
    # The log message parser requires a "clean slate".
    current_time = Time.now.utc.iso8601
    mover_log_file_glob = "/var/log/opscode/opscode-chef-mover/console.log*"
    parsed_log_output = "/var/log/opscode/opscode-chef-mover/parsed_console.log"
    run_command("mkdir /var/log/opscode/opscode-chef-mover/#{current_time}")
    begin
      run_command("mv #{mover_log_file_glob} /var/log/opscode/opscode-chef-mover/#{current_time}")
    rescue
      log "No files found at #{mover_log_file_glob}. Moving on..."
    end

    begin
      run_command("mv #{parsed_log_output} /var/log/opscode/opscode-chef-mover/#{current_time}")
    rescue
      log "#{parsed_log_output} not found. Moving on..."
    end

    ####
    #### perform a migration similar to what we did for hosted chef following this plan
    #### github.com/opscode/chef-mover/blob/024875c5545a0e7fb62c0852d4498d2ab7dd1c1d/docs/phase-2-migration-plan.md
    ####

    # Bring up chef-mover for the duration of the migration
    log "Firing up chef-mover, this could take a minute..."
    run_command("private-chef-ctl restart opscode-chef-mover")
    sleep(60)

    # Run phase_2_migration
    log "Migrating containers and groups..."
    run_command("/opt/opscode/embedded/bin/escript " \
                "/opt/opscode/embedded/service/opscode-chef-mover/scripts/migrate mover_phase_2_migration_callback normal")

    # We don't need chef-mover anymore
    run_command("private-chef-ctl stop opscode-chef-mover")

    # Clean up chef_*.couch files, we don't need them anymore! (should already be backed up too)
    log "Cleaning up containers and groups from couchDB..."
    run_command("find /var/opt/opscode/couchdb/db -name 'chef_*.couch' | xargs rm")
    run_command("rm -rf /var/opt/opscode/couchdb/db/.chef_*_design")

    stop_service('postgresql')

    log "Containers and groups migration complete!"
  end
end
