#!/bin/bash

#===============================================================================
#Downloading the automate repo
#===============================================================================

git clone https://github.com/chef/automate.git
cd automate
git checkout kalroy/cs_plan_changes

#===============================================================================

#running the chef_server.sh script from the automate repo
./integration/tests/chef_server.sh