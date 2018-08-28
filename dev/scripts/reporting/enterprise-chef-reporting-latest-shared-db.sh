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

# Assumes Chef Server Latest is installed.
set -e
echo "Chef Server, Reporting Latest, Shared Chef Server DB"

source reporting-functions.sh
prepare_all

install_enterprise_chef_server
dpkg -i /vagrant/$REPORTING_LATEST

enterprise_reconfigure_and_test
