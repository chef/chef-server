#!/bin/bash

set -e
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
# CHANGELOG_GITHUB_TOKEN Obtain one here: https://github.com/settings/tokens/new?description=GitHub%20Changelog%20Generator%20token

basedir=$(dirname $0)
currentdir=$(pwd)

chef_version=$(grep build_version $basedir/../../omnibus/config/projects/chef-server.rb | cut -d\" -f2)

usage() {
    echo "Usage: "
    echo " ./update-changelog path/to/previous/version/chef-server.deb"
    echo ""
    echo " CHANGELOG_GITHUB_TOKEN must be set to valid github token. Go "
    echo " here to obtain one: https://github.com/settings/tokens/new?description=GitHub%20Changelog%20Generator%20token."
    exit 1
}

if [[ -z "$CHANGELOG_GITHUB_TOKEN" ]]; then
    usage
fi

if [ "$#" -ne 1 ]; then
    usage
fi

pushd $basedir/../../omnibus
bundle install --with=release
bundle exec github_changelog_generator -u chef -p chef-server -t $CHANGELOG_GITHUB_TOKEN --enhancement-labels "enhancement,Enhancement,New Feature" --bug-labels "bug,Bug,Improvement,Upstream Bug" --exclude-labels "duplicate,question,invalid,wontfix,no_changelog" -o $currentdir/NEW_CHANGELOG.md --future-release $chef_version
popd

ar p $1 data.tar.xz | tar x ./opt/opscode/version-manifest.json

cd $basedir/../../omnibus
bundle exec omnibus manifest chef-server -l fatal > $currentdir/version-manifest.json
bundle exec omnibus changelog generate --starting-manifest=$currentdir/opt/opscode/version-manifest.json --ending-manifest=$currentdir/version-manifest.json | grep -v "( -> )" > $currentdir/MODIFIED_COMPONENTS_CHANGELOG.md
