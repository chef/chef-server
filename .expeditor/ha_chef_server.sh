#!/bin/bash

#===============================================================================
#Downloading the automate repo
#===============================================================================

git clone https://github.com/chef/automate.git
cd automate
git checkout ssudheer/ruby_update_3.1

#===============================================================================

#running the ha_chef_server.sh script from the automate repo
chmod +x ./integration/tests/ha_chef_server.sh
integration/run_test integration/tests/ha_chef_server.sh