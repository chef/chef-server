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

    clean_mover_logs

    ####
    #### perform a migration similar to what we did for hosted chef following this plan
    #### github.com/opscode/chef-mover/blob/024875c5545a0e7fb62c0852d4498d2ab7dd1c1d/docs/phase-2-migration-plan.md
    ####

    force_restart_service("opscode-chef-mover")

    # Run phase_2_migration
    log "Migrating containers and groups..."
    run_command("/opt/opscode/embedded/bin/escript " \
                "/opt/opscode/embedded/service/opscode-chef-mover/scripts/migrate mover_phase_2_migration_callback normal")

    stop_service("opscode-chef-mover")

    # Clean up chef_*.couch files, we don't need them anymore! (should already be backed up too)
    log "Cleaning up containers and groups from couchDB..."
    run_command("find /var/opt/opscode/couchdb/db -name 'chef_*.couch' | xargs rm")
    run_command("rm -rf /var/opt/opscode/couchdb/db/.chef_*_design")

    stop_service('postgresql')

    log "Containers and groups migration complete!"
  end
end
