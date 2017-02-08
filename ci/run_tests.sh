#!/bin/sh
set -evx

sudo chef-server-ctl test -J $WORKSPACE/pedant.xml --all

if [ $OMNIBUS_FIPS_MODE == "true" ]
then
  echo "fips true" >> /etc/opscode/chef-server.rb
  sudo chef-server-ctl reconfigure
  echo ""
  echo "Sleeping 120 seconds to allow the Chef Server to reconfigure in FIPS mode"
  echo ""
  sleep 120
  sudo chef-server-ctl test -J $WORKSPACE/pedant-fips.xml
fi
