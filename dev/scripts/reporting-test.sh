#!/bin/bash
#
# Copyright 2015 Chef Software, Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# scripts/reporting.sh: Run basic reporting tests.
#
# This script assumes you are starting with a VM with Chef Server
# Latest installed and external DBs set up.
#
# Required Environment Variables
# -------------------------------
#
# Note that all files must exist in the dev directory!
# CHEF_SERVER_LATEST: name of the file for last release of the chef server .deb build
# CHEF_SERVER_LATEST_MINUS_1: name of the file for a N-1 release of the chef server .deb build
# ENTERPRISE_CHEF_SERVER_11: name of a file for private chef .deb build
# REPORTING_LAST_RELEASE: name of a file for .deb of last release of reporting
# REPORTING_LATEST: name of a file for .deb of reporting build you are testing
set -e

BLUE='\033[1;34m'
PURPLE='\033[0;35m'
NC='\033[0m'

send_ssh() {
    vagrant ssh-config chef-server > ssh_config.temp
    ssh -i .vagrant/machines/chef-server/virtualbox/private_key -F ssh_config.temp chef-server $1
}

get_env() {
    echo "CHEF_SERVER_LATEST=$CHEF_SERVER_LATEST\nCHEF_SERVER_LATEST_MINUS_1=$CHEF_SERVER_LATEST_MINUS_1\nENTERPRISE_CHEF_SERVER_11=$ENTERPRISE_CHEF_SERVER_11\nREPORTING_LAST_RELEASE=$REPORTING_LAST_RELEASE\nREPORTING_LATEST=$REPORTING_LATEST\nFORCE_INSTALLER=$FORCE_INSTALLER"
}

write_env_to_host() {
    REPORTING_ENV=`get_env`
    send_ssh  "printf \"$REPORTING_ENV\" > /tmp/reporting-env.sh"
}

run_ssh_script() {
    send_ssh "cd /vagrant/scripts/reporting; sudo chmod +x $1; sudo ./$1 > /tmp/reporting-test-logs" > /dev/null
}

on_failure() {
    if ! [ $? -eq 0 ]
    then
	echo "Looks like we crashed unexpectly."
	echo "The last script to run was: $LAST_SCRIPT_RUN"
	echo "Check the output of /tmp/reporting-test-logs on the vagrant host and /tmp/vagrant-output locally"
    fi
}

run_test_suite() {
    printf "+ ${PURPLE}Destroying vagrant VM${NC}\n"
    vagrant destroy -f >> /tmp/vagrant-output 2>&1
    printf "+ ${PURPLE}Bringing new vagrant VM up${NC}\n"
    vagrant up >> /tmp/vagrant-output 2>&1
    printf "+ ${PURPLE}Writing reporting testing environment to host${NC}\n"
    write_env_to_host >> /tmp/vagrant-output 2>&1
    printf "+ ${PURPLE}Setting software up and running tests on host${NC}\n"
    run_ssh_script $1 >> /tmp/vagrant-output 2>&1
}

prepare_config_yml() {
    echo "vm:" > config.yml
    echo "  postgresql:" >> config.yml
    echo "    start: true" >> config.yml
    echo "  reporting_postgresql:" >> config.yml
    echo "    start: true" >> config.yml
}

welcome_message() {
    echo "Welcome to the Automated Reporting Testing Tool."
    echo "This tool tests various Chef Server / Reporting installation and upgrade scenarios."
    echo "To get started, you will need to put a build (.deb file) for each testing component into this directory:"
    pwd
    echo ""
    echo "You will need the proper .deb file in the directory listed above for each of the following:"
    echo "CHEF_SERVER_LATEST"
    echo "CHEF_SERVER_LATEST_MINUS_1"
    echo "ENTERPRISE_CHEF_SERVER_11"
    echo "REPORTING_LAST_RELEASE"
    echo "REPORTING_LATEST"
    echo ""
    echo "Lastly, you will need to set each of the above variables to the NAME (not the path) of each corresponding file. For example:"
    echo "export CHEF_SERVER_LATEST=chef-server-core_12.3.1-1_amd64.deb"
    echo ""
    echo "Do this for every file, put those files in the proper directory, and this tool will use DVM serially to run through all scenarios."
    echo "This will take some time, so go play some foosball."
}

# TODO: This unfortunately isn't catching failures
# coming back from ssh but a few hours of google and
# I decided it's not worth my time.
trap on_failure EXIT

cd ..

source ./scripts/reporting-test-preflight.sh

welcome_message

read -p "This script will destroy your current DVM environment and config.yml. Do you wish to proceed (y/n)? " -n 1 -r
echo    # (optional) move to a new line
if ! [[ $REPLY =~ ^[Yy]$ ]]
then
    echo "You answered $REPLY. Exiting."
    exit 0
fi

rm -f /tmp/vagrant-output

prepare_config_yml

printf "Firing up! Your reporting test environment looks like:\n\n"
printf `get_env`
printf "\n\n"
echo "Each suite will take some time, so don't worry :)"
echo "If things look frozen, check the output of /tmp/reporting-test-logs on the vagrant host and /tmp/vagrant-output locally"

# always select the installer we set
export AUTOPACKAGE=1

# Chef Server Latest
export INSTALLER=$CHEF_SERVER_LATEST

printf "${BLUE}Chef Server Latest, Reporting Latest, Shared Chef Server DB${NC}\n"
run_test_suite "chef-server-reporting-latest-shared-db.sh"

printf "${BLUE}Chef Server Latest, Reporting Latest, Shared External DB${NC}\n"
run_test_suite "chef-server-reporting-latest-reporting-external-db.sh"

printf "${BLUE}Chef Server Latest, Reporting Latest, Independant External DBs${NC}\n"
run_test_suite "chef-server-reporting-latest-independant-external-dbs.sh"

# Chef Server Latest Minus 1
export INSTALLER=$CHEF_SERVER_LATEST_MINUS_1

printf "${BLUE}Chef Server Latest-1, Reporting Latest, Shared Chef Server DB${NC}\n"
run_test_suite "chef-server-reporting-latest-shared-db.sh"

printf "${BLUE}Chef Server Latest-1, Reporting Latest, Chef Server Internal DB, Reporting External DB${NC}\n"
run_test_suite "chef-server-reporting-latest-reporting-external-db.sh"

# Enterprise Chef 11 (still has to start with a Chef Server package since DVM)
printf "${BLUE}Enterprise Chef Server 11 Latest, Reporting Latest, Shared Chef Server DB${NC}\n"
run_test_suite "enterprise-chef-reporting-latest-shared-db.sh"

# Upgrade testing
export INSTALLER=$CHEF_SERVER_LATEST

printf "${BLUE}Chef Server Latest, Reporting Last Release Shared DB, Upgrade Reporting to Latest${NC}\n"
run_test_suite "chef-server-reporting-upgrade-shared-db.sh"

printf "${BLUE}All finished and successful!${NC}\n"
