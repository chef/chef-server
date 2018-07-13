#!/bin/bash -e

# TODO: not sure how to handle this. Sqitch bombs when it can't find the timezone
export TZ="UTC"

{{#if bind.database}}
  {{#eachAlive bind.database.members as |member|}}
    {{#if @last}}
HOST="{{member.sys.ip}}"
PORT="{{member.cfg.port}}"
USER="{{member.cfg.superuser_name}}"
PASS="{{member.cfg.superuser_password}}"
DB="bifrost"
    {{/if}}
  {{/eachAlive}}
{{else}}
HOST="{{cfg.postgresql.vip}}"
PORT="{{cfg.postgresql.port}}"
USER="{{cfg.sql_user}}"
PASS="{{cfg.sql_password}}"
DB="bifrost"
{{/if}}

PG_ARGS="--host "$HOST" --port "$PORT" --username "$USER""
export PGPASSWORD="$PASS"

# Wait until postgres is ready
until pg_isready $PG_ARGS --quiet; do :; done

# Create delivery db for sqitch to deploy to
createdb $PG_ARGS $DB "oc_bifrost"

# Install uuid-ossp extension
psql $PG_ARGS --command 'CREATE EXTENSION IF NOT EXISTS "uuid-ossp"' $DB

cd "{{pkg.path}}/schema" || exit
sqitch --quiet --engine pg deploy "db:pg://${USER}:${PASS}@${HOST}/$DB"
