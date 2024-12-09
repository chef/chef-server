#!/bin/bash

#===============================================================================
#Downloading the automate repo
#===============================================================================

git clone https://github.com/chef/automate.git
cd automate
git checkout ssudheer/ruby_update_3.1

#===============================================================================
chmod +x ./integration/tests/chef_server_only.sh
#running the chef_server_only.sh script from the automate repo
integration/run_test integration/tests/chef_server_only.sh