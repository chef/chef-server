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

prepare_all() {
    source /tmp/reporting-env.sh
    source reporting-dvm-preflight.sh
}

reconfigure_and_test() {
    echo "Testing with Chef Server $FORCE_INSTALL"
    reconfigure_all_cs
    sleep 120
    opscode-reporting-ctl test --all
    chef-server-ctl test
}

enterprise_reconfigure_and_test() {
    echo "Testing with Chef Server $FORCE_INSTALL"
    private-chef-ctl reconfigure
    opscode-reporting-ctl reconfigure
    sleep 120
    opscode-reporting-ctl test --all
    private-chef-ctl test
}

install_enterprise_chef_server() {
    chef-server-ctl cleanse yes --with-external
    apt-get remove chef-server-core -y
    dpkg -i /vagrant/$ENTERPRISE_CHEF_SERVER_11
    private-chef-ctl reconfigure
}
