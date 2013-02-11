#!/usr/bin/env bash

# Must be run from the 'schema' directory of the oc_authz project!

psql --dbname postgres --command 'DROP DATABASE authz;'
psql --dbname postgres --command 'CREATE DATABASE authz;'
psql --dbname authz --command 'CREATE EXTENSION pgtap;'
psql --dbname authz --file sql/authz.sql
psql --dbname authz --file sql/authz_debug.sql
