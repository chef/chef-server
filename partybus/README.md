# PartyBus

Making Omnibus better, one step at a time.

## Global Upgrade Level

The upgrade level is independent of the Private Chef version number. Upgrades are either classified as a "major" upgrade or a "minor" one.

**major:** Requiring either a change in schema, a database migration, or a restart to runit or keepalived 

**minor:** Not major, e.g. single service restarts, document fixes

The current OPC upgrade version is stored in a per-host file: `/var/opt/opscode/partybus.version`

## Schema Upgrades

### Example

```ruby
upgrade_schema 10
upgrade_schema 11
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

To initiate maintenance mode

### Open Questions

How do we load balance between the front ends? VIP => nginx => front-end pools? The answer affects the order in which we run upgrades / migrations on the front ends.