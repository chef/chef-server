#!/usr/bin/env bash

# Must be run from the 'schema' directory of the oc_authz project!

if [[ -z $1 ]]
then
    DB_NAME="authz"
else
    DB_NAME=${1}
fi

echo "Dropping and recreating database '${DB_NAME}'"
psql --dbname postgres --command "DROP DATABASE ${DB_NAME};"
psql --dbname postgres --command "CREATE DATABASE ${DB_NAME};"
psql --dbname ${DB_NAME} --command 'CREATE EXTENSION pgtap;'
psql --dbname ${DB_NAME} --file sql/authz.sql
psql --dbname ${DB_NAME} --file sql/authz_debug.sql
