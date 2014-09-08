## Org Related Migrations

1. Do all of the below in pre-prod and perform much validation.
2. Resolve any pre-prod failures (or at least ensure they are benign). Most recent list [here](https://gist.github.com/tylercloke/fb9e2715144a9100204a), but this should be updated with post pre-prod refresh data.

### Prep Work

#### Upgrade schema

1. Update env databag to point at latest enterprise-chef-server-schema
2. Bump prod data bag, log onto `role:chef-pgsql` boxes sequentially and:
   + `cd /srv/enterprise-chef-server-schema/ && EC_TARGET=@2.4.0 OSC_TARGET=@1.0.4 DB_USER=opscode_chef make deploy`

### Orgs Downtime Work

#### Take Org Related Requests Offline

0. Log onto proper `role:couchdb` box, deploy chef-mover, and start mover console.
   + Update env databag with latest chef-mover and ccr `role:couchdb`
   + Start mover console via `/srv/chef_mover/current/bin$ sudo ./mover console`
1. Put orgs in 503 mode:
   + DOWNTIME STARTS ON PUT, POST, DELETE ON ORGS / ASSOCS / INVITES AS SOON AS YOU RUN THE BELOW COMMAND!
   + Mover console: `mover_org_darklaunch:orgs_503_endpoint_mode("true").`
   + Verify that expected POST, PUT, DELETE requests return 503
2. Now that there are no new org requests coming in, update the dets:
   + Mover console: `mover_manager:create_account_dets().`
3. Restart mover console after dets have loaded.
4. Set sleep_time to zero since orgs API is down.
   + `application:set_env(mover, sleep_time, 0).`
5. Run orgs migration.
   + Mover console: `mover_manager:migrate(all, 20, mover_org_migration_callback).`
6. Run org_user_association migration.
   + Mover console: `mover_manager:migrate(all, 20, mover_org_user_association_migration_callback).`
7. Run org_user_invites migration.
   + Mover console: `mover_manager:migrate(all, 20, mover_org_user_invites_migration_callback).`
8. Run global_groups migration.
   + Mover console: `mover_manager:migrate(all, 20, mover_global_groups_migration_callback).`
9. If all has gone well, flip API over to SQL:
   + Mover console: `mover_org_darklaunch:org_related_endpoints_to_couch("false").`
10. Turn orgs 503 mode off.
    + Mover console: `mover_org_darklaunch:orgs_503_endpoint_mode("false").`

### Resolution
+ Make sure there are no 500s and requests coming into erchef for org related endpoints.
+ If all goes to hell, run command to flip orgs back to couch:
  + Mover console: `mover_org_darklaunch:org_related_endpoints_to_couch("true").`

### Misc

For pre-prod testing, if you want to revert org related and gloabal_group data out of sql to retest (or if something went wrong and you need to start from scrach), in mover console:

`l(moser_chef_converter).`
`moser_chef_converter:delete_all_org_and_global_groups_data().`
