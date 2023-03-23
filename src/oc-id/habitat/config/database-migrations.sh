#!/bin/bash -e

# TODO: not sure how to handle this. Sqitch bombs when it can't find the timezone
export TZ="UTC"

{{#if bind.database}}
  {{#eachAlive bind.database.members as |member|}}
    {{#if @last}}
HOST="192.168.0.105"
PORT="5432"
USER="rpaul"
PASS="123456"
    {{/if}}
  {{/eachAlive}}
{{else}}
HOST="{{cfg.db.host}}"
PORT="{{cfg.db.port}}"
USER="{{cfg.db.user}}"
PASS="{{cfg.db.password}}"
{{/if}}
DB="{{cfg.db.name}}"

PG_ARGS="--host "$HOST" --port "$PORT" --username "$USER""
export PGPASSWORD="$PASS"

# Wait until postgres is ready
until pg_isready $PG_ARGS --quiet; do :; done

# Create delivery db for sqitch to deploy to
createdb $PG_ARGS $DB "{{cfg.db.user}}"

# Install uuid-ossp extension
psql $PG_ARGS --command 'CREATE EXTENSION IF NOT EXISTS "uuid-ossp"' $DB
