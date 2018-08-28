#!/bin/bash
#
# Copyright 2015-2018 Chef Software, Inc.
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
    echo "Reporting testing scripts must be run as root"
    exit 1
fi

REQUIRED_VARS_ERROR=""

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
