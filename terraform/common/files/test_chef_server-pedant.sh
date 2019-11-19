#!/bin/bash

set -evx

echo -e '\nBEGIN PEDANT TEST\n'

sudo chef-server-ctl test -J pedant.xml --all --compliance-proxy-tests

echo -e '\nEND PEDANT TEST\n'
