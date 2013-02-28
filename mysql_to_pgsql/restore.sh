#!/usr/bin/env bash
DB_USER=opscode_chef

RESTORE_TO=${RESTORE_TO:-opscode_chef}
DUMP_FILE=$1

if [ "x${DUMP_FILE}" == "x" ]; then
    echo "Usage: $0 <filename>"
    echo
    echo "  filename : SQL file to restore to database"
    exit 1
fi

echo "Restoring to database ${RESTORE_TO} from ${DUMP_FILE}"
time psql -h localhost -U $DB_USER -W ${RESTORE_TO} <  ${DUMP_FILE}
