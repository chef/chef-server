name "private-chef"
maintainer 'Opscode, Inc.'
homepage 'http://www.opscode.com'

replaces        "private-chef-full"
install_path    "/opt/opscode"
build_version   Omnibus::BuildVersion.new.semver
build_iteration 1

# overrides
override :postgresql, version: "9.1.9"
override "chef-gem", version: "11.4.0"
override :bundler, version: "1.1.5"
override :cacerts, version: "2014.01.28"
override :erlang, version: "R15B02"
override :gecode, version: "3.7.1"
override :keepalived, version: "1.1.20"
override :libpng, version: "1.5.13"
override "omnibus-ctl", version: "0.0.6"
override :preparation, version: "1.4.9"
override :redis, version: "2.4.7"
override :rebar, version: "2.0.0"

# global
dependency "preparation" # creates required build directories
dependency "chef-gem" # for embedded chef-solo
dependency "private-chef-cookbooks" # used by private-chef-ctl reconfigure
dependency "private-chef-scripts" # assorted scripts used by installed instance
dependency "private-chef-ctl" # additional project-specific private-chef-ctl subcommands
dependency "private-chef-administration"
dependency "nginx"
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
dependency "mysql2"

# monitoring
dependency "nagios"
dependency "nagios-plugins"
dependency "opscode-nagios-plugins"
dependency "nrpe"

# oc-chef-pedant for integration/smoke testing
dependency "oc-chef-pedant"

# partybus and upgrade scripts
dependency "partybus"
dependency "private-chef-upgrades"

# version manifest file
dependency "version-manifest"

exclude "\.git*"
exclude "bundler\/git"
