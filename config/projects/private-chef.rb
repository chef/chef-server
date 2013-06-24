name "private-chef"
maintainer "Opscode, Inc."
homepage "http://www.opscode.com"

replaces        "private-chef-full"
install_path    "/opt/opscode"
build_version   Omnibus::BuildVersion.new.semver
build_iteration 1

# creates required build directories
dependency "preparation"

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
dependency "postgresql"
dependency "redis"
dependency "rabbitmq"
dependency "opscode-solr"
dependency "opscode-expander"
dependency "chef-sql-schema" # needed to migrate the DB.
dependency "keepalived"
dependency "bookshelf"

# the front-end services
dependency "oc_erchef"
dependency "opscode-chef"
dependency "opscode-account"
dependency "opscode-webui"
dependency "opscode-authz"
dependency "opscode-org-creator"
dependency "opscode-certificate"
dependency "opscode-platform-debug"
dependency "opscode-test"

# oc-chef-pedant for integration/smoke testing
dependency "oc-chef-pedant"

# partybus and upgrade scripts
dependency "partybus"
dependency "private-chef-upgrades"

# version manifest file
dependency "version-manifest"

exclude "\.git*"
exclude "bundler\/git"
