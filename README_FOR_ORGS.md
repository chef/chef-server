## Org Migration Handbook

### Dev VM Usage

* follow the directions in the README.md to get your VM
  up and running, including applying the state tracking schema update.
* start console as described in README.md

### Migration Setup
* For a migration to run, it must have a state record present.
  Initialize states for all orgs in mover console.  At present you will
  see duplicate errors which can be ignored:

```
moser_state_tracker:capture_full_org_state_list().
```

* By default, orgs are marked as 'holding' - they must be explicitly
marked as ready for migration. The options below will do this, and will also
  delete any SQL records that exist for the org:

```
mover_util:reset_org(OrgName).

```
* To prepare multiple orgs:

```
mover_util:reset_orgs([Org1, Org2, ... OrgN]).
```
* To prepare orgs listed in a file (single line per org):

```
mover_util:reset_orgs_from_file("/path/to/file").
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

