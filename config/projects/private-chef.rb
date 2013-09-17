name "private-chef"
maintainer "Opscode, Inc."
homepage "http://www.opscode.com"

replaces        "private-chef-full"
install_path    "/opt/opscode"
build_version   Omnibus::BuildVersion.new.semver
build_iteration 1

# creates required build directories
dependency "preparation"

# needs to be before postgresql, otherwise build problems...
dependency "postgresql91" # for pg_upgrade

# global
dependency "chef-gem" # for embedded chef-solo
dependency "private-chef-cookbooks" # used by private-chef-ctl reconfigure
dependency "private-chef-scripts" # assorted scripts used by installed instance
dependency "private-chef-ctl" # additional project-specific private-chef-ctl subcommands
dependency "private-chef-administration"
dependency "openresty"
dependency "runit"
dependency "unicorn"

# the backend
dependency "couchdb"
dependency "postgresql92"
dependency "rabbitmq"
dependency "opscode-solr"
dependency "opscode-expander"

# We are transitioning away from Sequel toward Sqitch for managing
# Erchef's schema.  We still need the old code ('chef-sql-schema') for
# existing upgrades.  However, after Enterprise Chef 11's release,
# that will be removed entirely in favor of the new code
# ('enterprise-chef-server-schema').
dependency "chef-sql-schema" # EOL
dependency "enterprise-chef-server-schema"

dependency "keepalived"
dependency "bookshelf"

# migration tooling , which also lives on the backend
dependency "opscode-chef-mover"

# the front-end services
dependency "oc_erchef"
dependency "opscode-account"
dependency "opscode-webui"
dependency "oc_bifrost"
dependency "opscode-org-creator"
dependency "opscode-certificate"
dependency "opscode-platform-debug"
dependency "opscode-test"


# log management
dependency "logrotate"

# oc-chef-pedant for integration/smoke testing
dependency "oc-chef-pedant"

# partybus and upgrade scripts
dependency "partybus"
dependency "private-chef-upgrades"
dependency "oc_authz_migrator" # migrate authz to bifrost

# version manifest file
dependency "version-manifest"

exclude "\.git*"
exclude "bundler\/git"
