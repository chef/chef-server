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

# This uses a config file to find ourselves (and not hardcode our own package name)
# Could do relative to $0, but that can be messy sometimes
pkg_prefix=$(cat /hab/svc/chef-server-ctl/config/pkg_path)
cd "$pkg_prefix/omnibus-ctl"
export PATH=$PATH:$(hab pkg path "core/bundler")/bin:$(hab pkg path "core/ruby")/bin
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$(hab pkg path "core/libffi")/lib
export CHEF_SECRETS_DATA=$(cat /hab/svc/chef-server-ctl/config/hab-secrets-config.json)
bundle exec binstubs/chef-server-ctl "$@"
