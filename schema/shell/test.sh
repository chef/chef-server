#!/usr/bin/env bash

# Must be run from the 'schema' directory of the oc_authz project!

pg_prove --dbname authz --verbose test/*_test.sql
