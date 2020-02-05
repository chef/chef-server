#!/bin/bash

set -evx

echo -e '\nBEGIN SMOKE TEST\n'

if grep -q 'fips true' /etc/opscode/chef-server.rb; then
    sudo chef-server-ctl test -J pedant-fips.xml --smoke
else
    sudo chef-server-ctl test
fi

echo -e '\nEND SMOKE TEST\n'
