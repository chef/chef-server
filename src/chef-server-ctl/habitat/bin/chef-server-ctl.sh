#!/bin/bash -e
#
# Copyright 2012-2015 Chef Software, Inc.
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
export SVWAIT=30

pkg_prefix=__PKG_PATH__
cd "$pkg_prefix/omnibus-ctl"

RUBY_BIN_DIR=__RUBY_PATH__/bin

export PATH=$PATH:$RUBY_BIN_DIR
export GEM_PATH="$pkg_prefix/vendor/bundle"
CHEF_SECRETS_DATA=$(cat /hab/svc/chef-server-ctl/config/hab-secrets-config.json)
export CHEF_SECRETS_DATA
export CSC_KNIFE_CONFIG_FILE=/hab/svc/chef-server-ctl/config/pivotal.rb
export CSC_KNIFE_BIN="${RUBY_BIN_DIR}/bundle exec ${pkg_prefix}/chef/bin/knife"

bundle exec binstubs/chef-server-ctl "$@"
