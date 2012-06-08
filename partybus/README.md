# PartyBus

Making Omnibus better, one step at a time.

## Global Upgrade Level

The upgrade level is independent of the Private Chef version number. Upgrades are either classified as a "major" upgrade or a "minor" one.

**major:** Requiring either a change in schema, a database migration, or a restart to runit or keepalived 

**minor:** Not major, e.g. single service restarts, document fixes

The current OPC upgrade version is stored in a per-host file: `/var/opt/opscode/upgrades/migration-level`

## Upgrade Definition Structure

Upgrade definitions will be stored on the filesystem in the following fashion: #{major_ver}/#{minor_ver}_#{name}.rb. For example:

```
└── 001
    └── 001_first_upgrade.rb
    └── 002_second_upgrade.rb
└── 002
    └── 001_the_next_generation.rb
    └── 002_a_new_hope.rb
```

Upgrades are defined and checked into source control in the`files/private-chef-upgrades` folder of the `opscode-omnibus` project

## Top-Level Upgrade DSL

### Example

```ruby
define_upgrade 'its_a_party' do
  maintencance_mode do

    upgrade_schema_to 42
    migrate 'migrate_to_mongodb'
    migrate 'convert_lead_to_gold'

  end
end
```

### Considerations

The reason for adding a top-level DSL around upgrades is to future proof the API. If some change is required for one of the DSL methods (e.g. upgrade_schema_to), we need the ability to support both old versions of the upgrade syntax as well as newer versions. For example:

```ruby
define_upgrade 'its_a_futurama_party', :api_version => 2 do

   # upgrade definition here

end
```

## Schema Upgrades

### Example

```ruby
upgrade_schema_to 10
upgrade_schema_to 11
```

## Service Restarts

### Example

```ruby
restart_service 'opscode-chef'
restart_service 'opscode-erchef'
restart_service 'opscode-webui'
restart_service 'opscode-solr'
restart_service 'couchdb'
```

### Implementation Details

#### Cluster Affecting Restarts

Restarting processes that will affect the health of the cluser (e.g. keepalived, runit) will need to be handled more carefully. The plan for these upgrades is to force the user to stop keepalived on the secondary HA node before proceeding.

## Data Migrations

### Example

```ruby
# maintenance mode will most likely be required

migrate 'some_migration_yo.script'
migrate 'some_other_migration.script'
```

## Maintenance Mode (503)

### Example

```ruby
maintenance_mode do

  # schema upgrade

  # data migration

  # service restarts

end
```

### Implementation Details

Maintenance mode should be initiated by the master back end, and cleared by each of the front ends individually.

To initiate maintenance mode we intend to expose an API that touches a file, which nginx will read to determine whether the node should be serving API requests.

To stop maintenance mode, said file will be automatically deleted (on and by the front end) at the end of the upgrade process.

### Open Questions

How do we load balance between the front ends? VIP => nginx => front-end pools? The answer affects the order in which we run upgrades / migrations on the front ends.

## Upgrade Failures

We need to design the upgrader to gracefully handle failures that may occur during an upgrade. To handle this, the databases will need to be backed up to a location on the backend, and there should be an easy method to restore the databases from the backups.

### SQL

mysqldump / pgdump should be sufficient for most dump / restore scenarios.

### CouchDB

We don't intend to migrate data within CouchDB in such a fashion that we will need to back it up. In other words, we don't intend to make backwards incompatible changes to the CouchDB databases that will leave the data in a non-working state in the case that we need to revert back to the previous version of OPC.

### Solr

TBD

## Changelogs

TBD
