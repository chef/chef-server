#!/bin/bash

#===============================================================================
#Downloading the automate repo
#===============================================================================

git clone https://github.com/chef/automate.git
cd automate
git checkout kalroy/cs_plan_changes

#===============================================================================

#running the ha_chef_server.sh script from the automate repo
./integration/tests/ha_chef_server.sh