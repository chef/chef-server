name "server-complete"

skip_transitive_dependency_licensing true

license :project_license

# global

# libiconv (unicode character conversion library) was originally
# a dependency declared in several components out of omnibus-software.
# It was recently removed (2017Q1) from most of these components as a dep, and
# build flags were updated to not link it into the components.
#
# This led to chef-server no longer pulling it indirectly. This broke popt, which
# is defined upstream and pulled in by logrotate.
#
# TODO mp 2017-04-06 - a little further digging to see if the right answer is to
#                      conditionally add the dependency for popt on s390x.

dependency "libiconv"

dependency "private-chef-scripts" # assorted scripts used by installed instance

dependency "ctl-man" # install man page
dependency "openresty" # load balanacer
dependency "openresty-lpeg"  # lua-based routing
dependency "runit"
dependency "erlang-crypto2"
dependency "openssl-fips-config" if fips_mode? 

# the backend
dependency "postgresql92-bin" # for upgrading 9.2 -> 9.6
dependency "postgresql96"
dependency "rabbitmq"
dependency "redis" # dynamic routing controls
dependency "opscode-solr4"
dependency "haproxy"
dependency "opscode-expander"

# download the gpg-key beforehand for rhel systems to
# use when verifying add ons
dependency "gpg-key"

dependency "keepalived"
dependency "bookshelf"

# the front-end services
dependency "oc_bifrost"
dependency "oc_id"

# log management
dependency "logrotate"

# partybus and upgrade scripts
dependency "partybus"

# private-chef-ctl and its dependencies
dependency "chef"
dependency "libcs"
dependency "pg-gem"
dependency "private-chef-ctl"

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
