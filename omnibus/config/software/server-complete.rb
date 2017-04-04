name "server-complete"

skip_transitive_dependency_licensing true

license :project_license

# global
dependency "libiconv"
dependency "private-chef-scripts" # assorted scripts used by installed instance
dependency "private-chef-ctl" # additional project-specific private-chef-ctl subcommands
dependency "ctl-man" # install man page
dependency "openresty"
dependency "rb-readline"
dependency "redis-gem" # gem for interacting with redis
dependency "openresty-lpeg"  # lua-based routing
dependency "runit"
dependency "chef_backup-gem" # chef-server-ctl backup
dependency "veil-gem" # chef-server-ctl rotate-credentials
dependency "erlang-crypto2"

# the backend
dependency "postgresql92"
dependency "rabbitmq"
dependency "redis" # dynamic routing controls
dependency "opscode-solr4"
dependency "haproxy"
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
dependency "chef" # for embedded chef-client -z runs (built from master - build last)
