#!/bin/bash

set -evx

if [ "${ENABLE_PEDANT_TEST:-true}" = 'true' ]; then
    echo -e '\nBEGIN PEDANT TEST\n'

    sudo chef-server-ctl test -J pedant.xml --all --compliance-proxy-tests

    echo -e '\nEND PEDANT TEST\n'
else
    echo -e '\n**SKIP** PEDANT TEST **SKIP**\n'
fi
