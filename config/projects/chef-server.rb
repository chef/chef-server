name "chef-server"
maintainer "Chef Software, Inc."
homepage   "http://www.getchef.com"

package_name    "chef-server-core"
replaces        "private-chef"
install_dir     "/opt/opscode"
build_version   Omnibus::BuildVersion.new.semver
build_iteration 1

override :rebar, version: "2.0.0"
override :berkshelf2, version: "2.0.18"
override :rabbitmq, version: "3.3.4"
override :erlang, version: "R16B03-1"
override :'omnibus-ctl', version: "0.2.0"

# creates required build directories
dependency "preparation"

# global
dependency "chef-gem" # for embedded chef-solo
dependency "private-chef-scripts" # assorted scripts used by installed instance
dependency "private-chef-ctl" # additional project-specific private-chef-ctl subcommands
dependency "openresty"
dependency "redis-rb" # gem for interacting with redis
dependency "openresty-lpeg"  # lua-based routing
dependency "runit"

# the backend
dependency "postgresql92"
dependency "rabbitmq"
dependency "redis" # dynamic routing controls
dependency "opscode-solr4"
dependency "opscode-expander"

dependency "keepalived"
dependency "bookshelf"

# the front-end services
dependency "oc_bifrost"
dependency "opscode-platform-debug"
dependency "opscode-test"
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
dependency "enterprise-chef-server-schema"
dependency "private-chef-cookbooks"
dependency "chef-ha-plugin-config"

# version manifest file
dependency "version-manifest"

exclude "\.git*"
exclude "bundler\/git"
