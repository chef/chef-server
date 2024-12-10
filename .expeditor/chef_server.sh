#!/bin/bash -e

#===============================================================================
#Downloading the automate repo
#===============================================================================

git clone https://github.com/chef/automate.git
cd automate
if [ "${AUTOMATE_BRANCH}" != "" ]
then
  git checkout "${AUTOMATE_BRANCH}"
fi

#===============================================================================
chmod +x ./integration/tests/chef_server.sh
#running the chef_server.sh script from the automate repo
integration/run_test integration/tests/chef_server.sh