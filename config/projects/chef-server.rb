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

name "chef-server"
maintainer "Chef Software, Inc."
homepage   "https://www.chef.io"

package_name    "chef-server-core"
replace         "private-chef"
conflict        "private-chef"
install_dir     "/opt/opscode"
# last released version is 12.0.8
build_version   "12.0.9"
build_iteration 1

override :cacerts, version: '2014.08.20'
override :rebar, version: "2.0.0"
override :berkshelf2, version: "2.0.18"
override :rabbitmq, version: "3.3.4"
override :erlang, version: "R16B03-1"
override :ruby, version: "2.1.4"

# creates required build directories
dependency "preparation"

# global
dependency "chef" # for embedded chef-client -z runs
dependency "private-chef-scripts" # assorted scripts used by installed instance
dependency "private-chef-ctl" # additional project-specific private-chef-ctl subcommands
dependency "ctl-man" # install man page
dependency "openresty"
dependency "redis-gem" # gem for interacting with redis
dependency "openresty-lpeg"  # lua-based routing
dependency "runit"

# the backend
dependency "postgresql92"
dependency "rabbitmq"
dependency "redis" # dynamic routing controls
dependency "opscode-solr4"
dependency "opscode-expander"
dependency "pg-gem" # used by private-chef-ctl reconfigure

# Pull in knife-opc which is wrapped by chef-server-ctl to
# allow user to create users and orgs, and handle org associations
# without manage installed.
dependency "knife-opc-gem"

# download the gpg-key beforehand for rhel systems to
# use when verifying add ons
dependency "gpg-key"

dependency "keepalived"
dependency "bookshelf"

# the front-end services
dependency "oc_bifrost"
dependency "chef-server-bootstrap"
dependency "oc_id"

# log management
dependency "logrotate"

# partybus and upgrade scripts
dependency "partybus"

# used in osc to ec upgrade path
dependency "knife-ec-backup-gem"

# most frequently changed dependencies
# by placing these deps at the end of the build, we can take
# advantage of the git caching and increase build times
# for situations where we're changing these components.
# These are roughly sorted by build time and change frequency,
# with the quickest builds coming last.
dependency "opscode-chef-mover"
dependency "oc_erchef"
dependency "oc-chef-pedant"
dependency "private-chef-upgrades"
dependency "private-chef-cookbooks"
dependency "chef-ha-plugin-config"

package :rpm do
  signing_passphrase ENV['OMNIBUS_RPM_SIGNING_PASSPHRASE']
end

exclude "**/.git"
exclude "**/bundler/git"
