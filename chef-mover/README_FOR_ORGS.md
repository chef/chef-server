## Org Migration Handbook

### Dev VM Usage

* follow the directions in the README.md to get your VM
  up and running, including applying the state tracking schema update.
* start console as described in README.md

### Migration Setup
* Before you do anything, you need to load up the account file from couch:

```
moser_acct_processor:process_account_file().
```

* After you load the account file, you must restart the mover console to pick up the data.

* For a migration to run, it must have a state record present.
  Initialize states for all orgs in mover console. At present you will
  see duplicate errors which can be ignored:

```
moser_state_tracker:capture_full_org_state_list(mover_manager:get_account_dets(), *_migration_callback:migration_type()).
```

* By default, orgs are marked as 'holding' - they must be explicitly
marked as ready for migration. The options below will do this, and will also
  delete any SQL records that exist for the org:

```
mover_util:reset_org(OrgName, *_migration_callback:migration_type()).

```
* To prepare multiple orgs:

```
mover_util:reset_orgs([Org1, Org2, ... OrgN], *_migration_callback:migration_type()).
```
* To prepare orgs listed in a file (single line per org):

```
mover_util:reset_orgs_from_file("/path/to/file", *_migration_callback:migration_type()).
```
* If you need to mark an org as ready for migration without deleting
  existing SQL records, you can do so as follows:

```
moser_state_tracker:ready_migration(OrgName).
```
* You can confirm an org's state via:

```
moser_state_tracker:org_status(OrgName).
```

### Setting xdarklaunch flags for Unmigrated orgs

The migration_state table in Postgres needs to be updated
```
moser_state_tracker:capture_full_org_state_list().
```

After which, you run this command to populate redis with all the unmigrated orgs:
```
mover_util:populate_xdl_with_unmigrated_orgs().
```

NOTE: This will ONLY populate redis xdl flags for:
  - orgs marked as 'holding' or 'ready' in the state database
  - non-existant xdl flags for that org. (If a particular xdl flag exists for that org, it will not overwrite it).

### Migration Execution

To migrate a single org directly by name (note that it must be in
`ready` state via one of the methods described above):

```
mover_org_migrator_sup:start_org_migrator(OrgName).
```

To migrate the next available org ready for migration:

```
migrator_manager:migrate_next().
```

To migrate many orgs marked as ready for migration:

```
migrator_manager:migrate(Count, NumWorkers).
```

Count can be either a count of orgs, or the atom 'all' to migrate all
orgs marked as ready for migration.

