## Org Related Migrations

NOTE: RUN EVERYTHING IN A TMUX SESSION!!!!!!!!!

1. Do all of the below in pre-prod and perform much validation.
2. Resolve any pre-prod failures (or at least ensure they are benign).

### Prep Work

+ Make sure we have latest orgmapper in production.

#### Upgrade schema

1. Update env databag to point at latest enterprise-chef-server-schema
2. Bump prod data bag, log onto `role:chef-pgsql` boxes sequentially and:
   + `cd /srv/enterprise-chef-server-schema/ && EC_TARGET=@2.4.0 OSC_TARGET=@1.0.4 DB_USER=opscode_chef make deploy`

##### Cleanup Duplicate Orgs

Log into `role:opscode-support` and:

```
cd /srv/opscode-platform-debug/current/orgmapper
sudo bin/orgmapper /etc/chef/orgmapper.conf
```

###### First, get a list of all the duplicate orgs

```
load 'scripts/remove_duplicate_org.rb'
couchdb_uri = OrgMapper.configure { |x| x.couchdb_uri }
db = CouchRest.database("#{couchdb_uri}/opscode_account")
orgs = db.view("#{OrgMapper::CouchSupport::Organization}/all", { :include_docs=>true }); 1
orgnames = orgs['rows'].map {|x| x["doc"]["name"] }; 1
dups = orgnames.group_by { |e| e }.select { |k, v| v.size > 1 }.map(&:first)
```
###### Then delete the duplicates

```
deleteme = dups.map {|x| guid_to_delete(x) }.flatten
deleteme.each {|x| OrgMapper::CouchSupport.delete_account_doc(x[:id],x[:rev] ) }
OR
deleteme.each {|x| OrgMapper::CouchSupport.delete_account_doc(x[:id]) }

```

##### Cleanup Unique Groupname,OrgID Violations <a name="unique_groups"></a>

```
load 'scripts/fix_duplicate_global_admins.rb'
couchdb_uri = OrgMapper.configure { |x| x.couchdb_uri }
db = CouchRest.database("#{couchdb_uri}/opscode_account")
groups = db.view("#{OrgMapper::CouchSupport::Group}/all", { :include_docs=>true }); 1
orgnames = groups['rows'].map {|x| x["doc"]["orgname"]};1
dups = orgnames.group_by { |e| e }.select { |k, v| v.size > 1 }.map(&:first).sort
dups.each {|org| fixup_global_admins(org) }
AND ONCE THAT WORKS
dups.each {|org| fixup_global_admins(org, dryrun=false) }
```

### Orgs Downtime Work

#### Final Pre-Flight Check

Make sure that there haven't been any new errors introduced since the prep work cleanup.

##### Final Check For Duplicate Orgs

```
couchdb_uri = OrgMapper.configure { |x| x.couchdb_uri }
db = CouchRest.database("#{couchdb_uri}/opscode_account")
orgs = db.view("#{OrgMapper::CouchSupport::Organization}/all", { :include_docs=>true }); 1
orgnames = orgs['rows'].map {|x| x["doc"]["name"] }; 1
dups = orgnames.group_by { |e| e }.select { |k, v| v.size > 1 }.map(&:first)
```

`dups` should be have no elements, if that is not the case, run the dup orgs cleanup from above one last time.

##### Final Check For Unique Gloabl Admins

```
couchdb_uri = OrgMapper.configure { |x| x.couchdb_uri }
db = CouchRest.database("#{couchdb_uri}/opscode_account")
groups = db.view("#{OrgMapper::CouchSupport::Group}/all", { :include_docs=>true }); 1
orgnames = groups['rows'].map {|x| x["doc"]["orgname"]};1
dups = orgnames.group_by { |e| e }.select { |k, v| v.size > 1 }.map(&:first).sort
```

Again, `dups` should have zero elements, if that is not the case, run the cleanup in dryrun mode (the default) and then cleanup any remaining issues pre-final downtime.

#### Take Org Related Requests Offline

0. Log onto proper `role:couchdb` box, nuke dets, deploy chef-mover, and start mover console.
   + SSH into `role:couchdb`
   + Nuke dets so we can rebuild from scratch:
     - `sudo rm /srv/moser-data/{association_requests,global_containers,global_groups,org_id_to_guid,orgname_to_guid,orgs_by_guid,org_user_associations,user_to_authz,account_db,authz_to_user}`
   + Update env databag with latest chef-mover and ccr `role:couchdb`
   + Start mover console via `/srv/chef_mover/current/bin$ sudo ./mover console`
1. Turn off org creator:
   + Mover console: `mover_org_darklaunch:disable_org_creation().` 
2. Put orgs in 503 mode:
   + DOWNTIME STARTS ON PUT, POST, DELETE ON ORGS / ASSOCS / INVITES AS SOON AS YOU RUN THE BELOW COMMAND!
   + Mover console: `mover_org_darklaunch:orgs_503_endpoint_mode("true").`
   + Verify that expected POST, PUT, DELETE requests return 503
3. Now that there are no new org requests coming in, update the dets:
   + Mover console: `mover_manager:create_account_dets().`
4. Restart mover console after dets have loaded.
5. Set sleep_time to zero since orgs API is down.
   + `application:set_env(mover, sleep_time, 0).`
6. Run orgs migration.
   + Mover console: `mover_manager:migrate(all, 20, mover_org_migration_callback).`
7. Run org_user_association migration.
   + Mover console: `mover_manager:migrate(all, 20, mover_org_user_association_migration_callback).`
8. Run org_user_invites migration.
   + Mover console: `mover_manager:migrate(all, 20, mover_org_user_invites_migration_callback).`
9. Run global_groups migration.
   + Mover console: `mover_manager:migrate(all, 20, mover_global_groups_migration_callback).`
10. If all has gone well, flip API over to SQL:
   + Mover console: `mover_org_darklaunch:org_related_endpoints_to_couch("false").`
11. hEC should now be reading from SQL! Watch traffic in read only mode for awhile while in SQL to make sure things are functional and running smoothy. This is our last chance to turn back to couch without data loss (see resolution on how to flip back to couch if worst comes to worst). Once we are confident that read only is working well in SQL, turn orgs 503 mode off.
    + First, for a few minutes, make sure that requests to SQL read only for orgs are working and there is no increase in 500s. Validate that you can read orgs / invites / assocs.
    + Turn off opscode-account and verify that things still work.
      - `knife ssh role:opscode-account 'sudo /etc/init.d/opscode-account stop'`
    + Turn off couchdb and verify that things still work.
      - TODO: Investigate how to take down HA couch.
    + Once we are confident that org / users / assoc reads are going well out of SQL, turn on write again. This is the LAST POINT of turning back so be confident :)
      - Mover console: `mover_org_darklaunch:orgs_503_endpoint_mode("false").`

### Resolution
+ Make sure there are no 500s and requests coming into erchef for org related endpoints.
+ If all goes to hell, run command to flip orgs back to couch. Note that this WILL result in data loss if 503 mode was ever turned off while we were in SQL, so this should only really be used if we decide things are broken while in read only mode:
  + Mover console: `mover_org_darklaunch:org_related_endpoints_to_couch("true").`
