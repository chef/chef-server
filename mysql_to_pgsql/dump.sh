#!/usr/bin/env bash
#
# Dump out a database from mysql such that we can
# insert it into OPC/OHC postgres database
#

DB_USER=root

#
# We ignore reporting for now since the JSON in the tables
# and we don't need opc user/customer tables since the apps
# that use them aren't migrating
#
# IGNORES="--ignore-table=opscode_chef.node_run \
#          --ignore-table=opscode_chef.node_run_detail \
#          --ignore-table=opscode_chef.reporting_schema_info \
#          --ignore-table=opscode_chef.opc_users \
#          --ignore-table=opscode_chef.opc_customers \
#          --ignore-table=opscode_chef.users"

DUMP_NAME="prod-`date '+%Y%m%d-%H%M%S'`.dump"

echo "Creating $DUMP_NAME"

#
# SED transforms
# 1.   convert binary data to bytea using inbuild decode() function
#
time mysqldump \
    -u${DB_USER} \
    --skip-quote-names \
    --hex-blob \
    --skip-triggers \
    --compact \
    --compatible=postgresql \
    --no-create-info \
    --complete-insert \
    opscode_chef \
    nodes | sed "s/,0x\([0-9A-F]*\)/,decode('\1','hex')/g" \
          > $DUMP_NAME

#
# SED transforms
# 1,2. convert admin field (last field of users) from 0/1 -> false/true
# 3.   convert a bad username which has \' in it.  Note, there a dup name if we
#      just remove the \' so we just replace with something we can fix later
# 4.   enable escaped string insert on the public_key field for newline escaping
# 5.   decode the hex-dumped serialized object and re-encode it as an escaped string
#
time mysqldump \
    -u${DB_USER} \
    --skip-quote-names \
    --hex-blob \
    --skip-triggers \
    --compact \
    --compatible=postgresql \
    --no-create-info \
    --complete-insert \
    opscode_chef \
    users | sed 's/,0)/,false)/g' \
          | sed 's/,1)/,true)/g' \
          | sed "s/\\\'/XXX/g" \
          | sed "s/'-----BEGIN/E&/g" \
          | sed "s/,0x\([0-9A-F]*\)/,encode(decode('\1','hex'),'escape')/g" \
          >> $DUMP_NAME

# TODO: handle errors on both dumps
if [ $? -ne 0 ]; then
    echo "Error downloading schema dump"
    exit 1
fi
