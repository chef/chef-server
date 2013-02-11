#!/usr/bin/env bash

# Must be run from the 'schema' directory of the oc_authz project!

# Runs all files in the 't' directory with the 'pg' extension
pg_prove --dbname authz --verbose --recurse
