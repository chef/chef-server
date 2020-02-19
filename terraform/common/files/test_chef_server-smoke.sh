#!/bin/bash

set -evx

if [ "${ENABLE_SMOKE_TEST:-true}" = 'true' ]; then
    echo -e '\nBEGIN SMOKE TEST\n'

    if sudo grep -q 'fips true' /etc/opscode/chef-server.rb; then
        sudo chef-server-ctl test -J pedant-fips.xml --smoke
    else
        sudo chef-server-ctl test
    fi

    echo -e '\nEND SMOKE TEST\n'
else
    echo -e '\n**SKIP** SMOKE TEST **SKIP**\n'
fi
