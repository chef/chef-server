#!/bin/bash

set -evx

if [ "${ENABLE_GATHER_LOGS_TEST:-true}" = 'true' ]; then
    echo -e '\nBEGIN GATHER LOGS TEST\n'

    sudo chef-server-ctl gather-logs

    echo -e '\nEND GATHER LOGS TEST\n'
else
    echo -e '\n**SKIP** GATHER LOGS TEST **SKIP**\n'
fi
