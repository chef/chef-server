#!/bin/bash
#
# Copyright 2012-2014 Chef Software, Inc.
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
# CHEF_SERVER_LATEST: path to last release of the chef server .deb build
# CHEF_SERVER_LATEST_MINUS_1: path to a N-1 release of the chef server .deb build
# CHEF_SERVER_11: path to an open source server 11 .deb build
# ENTERPRISE_CHEF_SERVER_11: path to a private chef .deb build
# REPORTING_LAST_RELEASE: path to .deb of last release of reporting
# REPORTING_LATEST: path to .deb of reporting build you are testing
set -e

if [ "$EUID" -ne 0 ]; then
    echo "Please run this script as root"
    exit 1
fi

REQUIRED_VARS_ERROR=""

if [ -z "$CHEF_SERVER_LATEST" ];
then
    REQUIRED_VARS_ERROR="${REQUIRED_VARS_ERROR}CHEF_SERVER_LATEST unset and is required.\n"
fi

if [ -z "$CHEF_SERVER_LATEST_MINUS_1" ];
then
    REQUIRED_VARS_ERROR="${REQUIRED_VARS_ERROR}CHEF_SERVER_LATEST_MINUS_1 unset and is required.\n"
fi

if [ -z "$CHEF_SERVER_11" ];
then
    REQUIRED_VARS_ERROR="${REQUIRED_VARS_ERROR}CHEF_SERVER_11 unset and is required.\n"
fi

if [ -z "$ENTERPRISE_CHEF_SERVER_11" ];
then
    REQUIRED_VARS_ERROR="${REQUIRED_VARS_ERROR}ENTERPRISE_CHEF_SERVER_11 unset and is required.\n"
fi

if [ -z "$REPORTING_LAST_RELEASE" ];
then
    REQUIRED_VARS_ERROR="${REQUIRED_VARS_ERROR}REPORTING_LAST_RELEASE unset and is required.\n"
fi

if [ -z "$REPORTING_LATEST" ];
then
    REQUIRED_VARS_ERROR="${REQUIRED_VARS_ERROR}REPORTING_LATEST unset and is required."
fi

if [ -n "$REQUIRED_VARS_ERROR" ];
then
    echo -e $REQUIRED_VARS_ERROR
    exit 1;
fi

# Write Config Files

# External Server Config 1)

echo "postgresql['db_superuser'] = \"bofh\"" > /tmp/external-postgres-1.rb
echo "postgresql['db_superuser_password'] = \"i1uvd3v0ps\"" >> /tmp/external-postgres-1.rb
echo "postgresql['external'] = true" >> /tmp/external-postgres-1.rb
echo "postgresql['vip'] = \"192.168.33.150\"" >> /tmp/external-postgres-1.rb

# External Server Config 2)

echo "postgresql['db_superuser'] = \"bofh\"" > /tmp/external-postgres-2.rb
echo "postgresql['db_superuser_password'] = \"i1uvd3v0ps\"" >> /tmp/external-postgres-2.rb
echo "postgresql['external'] = true" >> /tmp/external-postgres-2.rb
echo "postgresql['vip'] = \"192.168.33.155\"" >> /tmp/external-postgres-2.rb


cleanse_all_cs() {
    opscode-reporting-ctl cleanse yes --with-external
    chef-server-ctl cleanse yes --with-external
}

ensure_config_dirs_exist() {
    mkdir -p /etc/opscode
    mkdir -p /etc/opscode-reporting
}

reconfigure_all_cs() {
    chef-server-ctl reconfigure
    opscode-reporting-ctl reconfigure
}

echo "Chef Server Latest, Reporting Latest"
dpkg -i $REPORTING_LATEST
reconfigure_all_cs

echo "Chef Server Latest, Reporting Latest, Shared Chef Server DB"
sleep 120
opscode-reporting-ctl test --all
chef-server-ctl test

echo "Chef Server Latest, Reporting Latest, Shared External DB"
cleanse_all_cs
ensure_config_dirs_exist
cp /tmp/external-postgres-1.rb /etc/opscode/chef-server.rb
cp /tmp/external-postgres-1.rb /etc/opscode-reporting/opscode-reporting.rb
reconfigure_all_cs
sleep 120
opscode-reporting-ctl test --all
chef-server-ctl test

echo "Chef Server Latest, Reporting Latest, Independant External DBs"
cleanse_all_cs
ensure_config_dirs_exist
cp /tmp/external-postgres-1.rb /etc/opscode/chef-server.rb
cp /tmp/external-postgres-2.rb /etc/opscode-reporting/opscode-reporting.rb
reconfigure_all_cs
sleep 120
opscode-reporting-ctl test --all
chef-server-ctl test

echo "Chef Server Latest-1, Reporting Latest"
cleanse_all_cs
apt-get remove chef-server-core -y
dpkg -i $CHEF_SERVER_LATEST_MINUS_1

echo "Chef Server Latest-1, Reporting Latest, Shared Chef Server DB"
rm -f /etc/opscode/chef-server.rb
rm -f /etc/opscode-reporting/opscode-reporting.rb
chef-server-ctl reconfigure
opscode-reporting-ctl reconfigure
sleep 120
opscode-reporting-ctl test --all
chef-server-ctl test

echo "Chef Server Latest-1, Reporting Latest, Chef Server Internal DB, Reporting External DB"
cleanse_all_cs
ensure_config_dirs_exist
cp /tmp/external-postgres-1.rb /etc/opscode-reporting/opscode-reporting.rb
reconfigure_all_cs
sleep 120
opscode-reporting-ctl test --all
chef-server-ctl test

echo "Enterprise Chef Server 11 Latest, Reporting Latest"
cleanse_all_cs
apt-get remove chef-server-core -y
dpkg -i $ENTERPRISE_CHEF_SERVER_11

echo "Enterprise Chef Server 11 Latest, Reporting Latest, Shared Chef Server DB"
rm -f /etc/opscode/chef-server.rb
rm -f /etc/opscode-reporting/opscode-reporting.rb
private-chef-ctl reconfigure
opscode-reporting-ctl reconfigure
sleep 120
opscode-reporting-ctl test --all
private-chef-ctl test

echo "Enterprise Chef Server 11 Latest, Reporting Latest, Internal Chef Server DB, External Reporting DB"
opscode-reporting-ctl cleanse yes --with-external
private-chef-ctl cleanse yes
ensure_config_dirs_exist
rm -f /etc/opscode/chef-server.rb
cp /tmp/external-postgres-1.rb /etc/opscode-reporting/opscode-reporting.rb
private-chef-ctl reconfigure
opscode-reporting-ctl reconfigure
sleep 120
opscode-reporting-ctl test --all
private-chef-ctl test

echo "Chef Server Latest, Reporting Last Release Shared DB, Upgrade Reporting to Latest"
opscode-reporting-ctl cleanse yes --with-external
private-chef-ctl cleanse yes
ensure_config_dirs_exist
rm -f /etc/opscode/chef-server.rb
rm -f /etc/opscode-reporting/opscode-reporting.rb
apt-get remove chef-server-core -y
dpkg -i $CHEF_SERVER_LATEST
apt-get remove opscode-reporting -y
dpkg -i $REPORTING_LAST_RELEASE
chef-server-ctl reconfigure
opscode-reporting-ctl reconfigure
sleep 120
opscode-reporting-ctl test --all
chef-server-ctl test
dpkg -i $REPORTING_LATEST
chef-server-ctl reconfigure
opscode-reporting-ctl reconfigure
sleep 120
opscode-reporting-ctl test --all
chef-server-ctl test
