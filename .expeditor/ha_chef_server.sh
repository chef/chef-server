#!/bin/bash -e

#===============================================================================
#Downloading the automate repo
#===============================================================================

git clone https://github.com/chef/automate-private.git
cd automate
if [ "${AUTOMATE_BRANCH}" != "" ]
then
  git checkout "${AUTOMATE_BRANCH}"
fi

#===============================================================================

#running the ha_chef_server.sh script from the automate repo
chmod +x ./integration/tests/ha_chef_server.sh
integration/run_test integration/tests/ha_chef_server.sh