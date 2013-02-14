#!/usr/bin/env bash

RESTORE_TO=${RESTORE_TO:-opscode_chef}
DUMP_FILE=$1

if [ "x${DUMP_FILE}" == "x" ]; then
    echo "Usage: $0 <filename>"
    echo
    echo "  filename : SQL file to restore to database"
    exit 1
fi

echo "Restoring to database ${RESTORE_TO} from ${DUMP_FILE}"
time psql ${RESTORE_TO} <  ${DUMP_FILE}
