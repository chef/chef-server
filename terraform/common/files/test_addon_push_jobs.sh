#!/bin/bash

set -evx

echo -e '\nBEGIN TEST PUSH JOBS ADDON\n'

sudo opscode-push-jobs-server-ctl test

echo -e '\nEND TEST PUSH JOBS ADDON\n'
