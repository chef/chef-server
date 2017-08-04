#!/bin/bash
set -evx

echo "Sleeping even longer (120 seconds) to let the system settle"
sleep 120

sudo chef-server-ctl test -J $WORKSPACE/pedant.xml --all --compliance-proxy-tests

if [ "$OMNIBUS_FIPS_MODE" == "true" ]
then
  sudo sh -c 'echo -e "\nfips true" >> /etc/opscode/chef-server.rb'
  sudo chef-server-ctl reconfigure
  echo ""
  echo "Sleeping 120 seconds to allow the Chef Server to reconfigure in FIPS mode"
  echo ""
  sleep 120
  sudo chef-server-ctl test -J $WORKSPACE/pedant-fips.xml --smoke
fi
