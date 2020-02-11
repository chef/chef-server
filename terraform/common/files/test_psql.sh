#!/bin/bash

set -evx

if [ "${ENABLE_PSQL_TEST:-true}" = 'true' ]; then
    echo -e '\nBEGIN PSQL TEST\n'

    sudo chef-server-ctl psql oc_erchef <<<"SELECT * FROM users; \q"
    sudo chef-server-ctl psql oc_erchef --write <<<"UPDATE users SET email='janedoe@example.com' WHERE username='janedoe'; \q"
    sudo chef-server-ctl psql oc_erchef <<<"SELECT * FROM users; \q"
    sudo chef-server-ctl psql oc_erchef --as-admin <<<"UPDATE users SET email='janedoeadmin@example.com' WHERE username='janedoe'; \q"
    sudo chef-server-ctl psql oc_erchef <<<"SELECT * FROM users; \q"

    echo -e '\nEND PSQL TEST\n'
else
    echo -e '\n**SKIP** PSQL TEST **SKIP**\n'
fi
