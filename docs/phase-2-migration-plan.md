## Phase 2 Migration plan (Containers / Groups)

To boot mover console on the couchdb box:

/srv/chef_mover/current/bin$ sudo ./mover console

1. Migrate select orgs (internal, customer) individually using
   chef-mover tooling (list of internal orgs https://gist.github.com/tylercloke/13a915be265e3b59ecde)
    1. build updated dets files
      + `mover_manager:create_account_dets().`
      + restart mover console.
    2. create migration state tracking records for each org to be migrated, this will put them in 'holding'.
      + to load account info
      	- rr("lib/*moser*/ebin/*").
	- AccountInfo = mover_manager:get_account_dets().
      + Then for each org
      	- `moser_state_tracker:insert_one_org(moser_acct_processor:expand_org_info(#org_info{org_name = <<"_ORG_NAME_HERE_">>, account_info = AccountInfo}), mover_phase_2_migration_callback:migration_type()).`
    3. Move all 'holding' to 'ready'
      + `moser_state_tracker:ready_remaining_migration(mover_phase_2_migration_callback:migration_type()).`
    4. perform migration normally, it will find all orgs we set to ready in the above step
      + `mover_manager:migrate(all, 8, mover_phase_2_migration_callback).`
2. Ensure all newly created orgs use the new component, while forcing
   all existing orgs to use the old component.
    1. shut off org precreation internally. (stop org-creator)
      + `knife ssh role:opscode-account 'sudo /etc/init.d/opscode-org-creator stop'`
    2. Disable external org creation (darklaunch flag)
      + `mover_org_darklaunch:disable_org_creation().'`
    3. Delete precreated orgs (can be done any time before re-enabling
       org creation job, since org capture in chef-mover should exclude
       precreated orgs.
       + `knife ssh role:opscode-support 'cd /srv/opscode-platform-debug/current/orgmapper && sudo /srv/opscode-platform-debug/current/orgmapper/scripts/delete-pre-created-orgs.rb /etc/chef/orgmapper.conf all'`
    4. build updated dets files
      + `mover_manager:create_account_dets().`
      + restart mover console.
    5. run `mover_phase_2_prep_migration` that will automatically process all
       existing orgs not already captured into ``org_migration_state`` and
       to set appropriate redis flag for each to explicitly force the org to
       use the old component.
      + Remove the sleep_time since we don't need it here
        - application:set_env(mover, sleep_time, 0).
      + `mover_manager:migrate(all, 1, mover_phase_2_prep_migration_callback).`
      + When finished, set the sleep time back
        - application:set_env(mover, sleep_time, 5000).
    6. update ``dl_default`` key in redis with a value to route any orgs
       without a specific value to the new component.
      + `knife ssh role:mysql-master '/opt/redis/bin/redis-cli HMSET dl_default couchdb_containers false couchdb_groups false'`
    7. Enable org-creator service which will now make orgs on new component
      + `knife ssh role:opscode-account 'sudo /etc/init.d/opscode-org-creator start'`
    8. Once a sufficient quantity of orgs exist, re-enable customer org
       creation via darklaunch.
      + `mover_org_darklaunch:enable_org_creation().`
3. Unmigrated orgs are a fixed list, and are now captured in
   ``org_migration_state`` table in a ``holding`` state - they must be set to
   ``ready`` and then migrated.
    1. Make sure sleep time is correct
      - application:set_env(mover, sleep_time, 5000).
    2. Update remaining orgs to be migrated from holding to ready.
      + `moser_state_tracker:ready_remaining_migration(mover_phase_2_migration_callback:migration_type()).`
    2. Migrate those frackers and burn some couches
      + `mover_manager:migrate(all, 8, mover_phase_2_migration_callback).`
4. Cleanup
   After some time / valiidation of the success of the migration, delete the couch files? Any other cleanup?
