#!/bin/bash

#===============================================================================
#Downloading the automate repo
#===============================================================================

git clone https://github.com/chef/automate.git
cd automate
git checkout kalroy/cs_plan_changes

echo "printing is_automate"
env | grep IS_AUTOMATE
#===============================================================================
chmod +x ./integration/tests/chef_server.sh
#running the chef_server.sh script from the automate repo
integration/run_test integration/tests/chef_server.sh