name "private-chef"

# initialize the dependencies
deps = []

# global
deps << "chef-pc"
deps << "private-chef-cookbooks"
deps << "nginx"
deps << "runit"
deps << "unicorn"

# the backend
deps << "couchdb"
deps << "postgresql"
deps << "redis"
deps << "rabbitmq"
deps << "opscode-solr"
deps << "opscode-expander"
deps << "mixlib-authorization" # needed to migrate the DB.

# the front-end services
deps << "opscode-erchef"
deps << "opscode-chef"
deps << "opscode-account"
deps << "opscode-webui"
deps << "opscode-authz"
deps << "opscode-org-creator"
deps << "opscode-certificate"
deps << "opscode-platform-debug"
deps << "opscode-test"

# monitoring
deps << "nagios"
deps << "nagios-plugins"
deps << "opscode-nagios-plugins"
deps << "nrpe"

dependencies deps

exclude "\.git*"
exclude "bundler\/git"
