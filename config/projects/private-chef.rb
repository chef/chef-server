name "private-chef"
maintainer "Chef Software, Inc."
homepage   "http://www.getchef.com"

replaces        "private-chef-full"
install_dir    "/opt/opscode"
build_version   Omnibus::BuildVersion.new.semver
build_iteration 1

override :rebar, version: "2.0.0"
override :berkshelf2, version: "2.0.18"
override :rabbitmq, version: "3.3.4"
override :erlang, version: "R16B03-1"
override :'omnibus-ctl', version: "0.1.1"

# creates required build directories
dependency "preparation"

# needs to be before postgresql, otherwise build problems...
dependency "postgresql91" # for pg_upgrade

# global
dependency "chef-gem" # for embedded chef-solo
dependency "private-chef-scripts" # assorted scripts used by installed instance
dependency "private-chef-ctl" # additional project-specific private-chef-ctl subcommands
dependency "openresty"
dependency "redis-rb" # gem for interacting with redis
dependency "openresty-lpeg"  # lua-based routing
dependency "runit"
dependency "unicorn"

# the backend
dependency "couchdb"
dependency "postgresql92"
dependency "rabbitmq"
dependency "redis" # dynamic routing controls
dependency "opscode-solr4"
dependency "opscode-expander"

# We are transitioning away from Sequel toward Sqitch for managing
# Erchef's schema.  We still need the old code ('chef-sql-schema') for
# existing upgrades.  However, after Enterprise Chef 11's release,
# that will be removed entirely in favor of the new code
# ('enterprise-chef-server-schema').
dependency "chef-sql-schema" # EOL
dependency "keepalived"
dependency "bookshelf"

# the front-end services
dependency "opscode-account"
dependency "oc_bifrost"
dependency "opscode-org-creator"
dependency "opscode-certificate"
dependency "opscode-platform-debug"
dependency "opscode-test"
dependency "oc_id"

# log management
dependency "logrotate"

# partybus and upgrade scripts
dependency "partybus"
dependency "oc_authz_migrator" # migrate authz to bifrost

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
dependency "enterprise-chef-server-schema"
dependency "private-chef-cookbooks"

# version manifest file
dependency "version-manifest"

exclude "\.git*"
exclude "bundler\/git"
