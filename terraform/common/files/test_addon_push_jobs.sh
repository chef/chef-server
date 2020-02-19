#!/bin/bash

set -evx

if [ "${ENABLE_ADDON_PUSH_JOBS:-true}" = 'true' ]; then
    echo -e '\nBEGIN TEST PUSH JOBS ADDON\n'

    sudo opscode-push-jobs-server-ctl test

    echo -e '\nEND TEST PUSH JOBS ADDON\n'
else
    echo -e '\n**SKIP** TEST PUSH JOBS ADDON **SKIP**\n'
fi
