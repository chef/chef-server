#!/usr/bin/env bash

# Must be run from the 'schema' directory of the oc_authz project!

# Runs all files in the 't' directory with the 'pg' extension
if [[ -z $1 ]]
then
    DB_NAME="authz"
else
    DB_NAME=${1}
fi

echo "Executing pgTAP tests in database '${DB_NAME}'"
pg_prove --dbname ${DB_NAME} --verbose --recurse
