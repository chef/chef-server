# This annotated Pedant configuration file details the various
# configuration settings available to you.  It is separate from the
# actual Pedant::Config class because not all settings have sane
# defaults, and not all settings are appropriate in all settings.

################################################################################
#

# The base_resource_url gets set whenever we are operating behind a proxy,
# notably in the automate-chef-server all in one installation
#
# base_resource_url "host_header"

# A unique identification string used to create orgs and users specific
# to a each single chef server's nodes' OS. Simply using "Process.pid"
# proved to not be unique enough when running pedant simultaneously
# on multiple nodes of the same chef server when the generated pedant
# config file could have been copied across during the setup of that
# chef server.
chef_server_uid = "chef-server_#{Process.pid}".downcase

# Specify a testing organization if you are testing a multi-tenant
# instance of a Chef Server (e.g., Private Chef, Hosted Chef).  If you
# are testing a single-tenant instance (i.e. Open Source Chef Server),
# DO NOT include this parameter
#
# Due to how the current org cache operates, it is best to use a
# unique name for your testing organization. If you do not use a
# unique name and run tests several times (destroying the organization
# between runs) you will likely get inconsistent results.
#
# If you wish Pedant to create the organization for you at test time,
# include the `:create_me => true` pair.  If you wish to use an
# existing organization for tests, you should supply a `:validator_key
# => "/full/path/to/key.pem"` pair
org({:name => "pedant_testorg_#{chef_server_uid}",
     :create_me => true})

validate_org_creation true

# org({:name => "existing_org",
#      :validator_key => "/etc/opscode/existing_org-validator.pem"})

# account internal URL
internal_account_url "http://127.0.0.1:9685"

# If you want Pedant to delete the testing organization when it is
# done, use this parameter.  Note that this only has an effect if
# Pedant also created the testing organization.
delete_org true

# You MUST specify the address of the server the API requests will be
# sent to.  Only specify protocol, hostname, and port.
{{~#if bind.chef-server-nginx}}
  {{~#eachAlive bind.chef-server-nginx.members as |member|}}
    {{~#if @last}}
chef_server "https://{{member.sys.ip}}:{{member.cfg.ssl-port}}"
    {{~/if}}
  {{~/eachAlive}}
{{~else}}
chef_server "https://{{cfg.chef_server_api.ip}}:{{cfg.chef_server_api.ssl_port}}"
{{~/if}}

# This configration specifies the default orgname. Note that it does *not*
# mean that Pedant will test things with default org urls. To do that,
# pass --use-default-org on the command line
default_orgname nil

# If you are doing development testing, you can specify the address of
# the Solr server.  The presence of this parameter will enable tests
# to force commits to Solr, greatly decreasing the amout of time
# needed for testing the search endpoint.  This is only an
# optimization for development!  If you are testing a "live" Chef
# Server, or otherwise do not have access to the Solr server from your
# testing location, you should not specify a value for this parameter.
# The tests will still run, albeit slower, as they will now need to
# poll for a period to ensure they are querying committed results.
search_server "{{cfg.pedant_config.search_server}}"

search_commit_url "/_refresh"
search_url_fmt "/chef/_search?q=X_CHEF_type_CHEF_X:%{type}%%20%{query}"

# Some tests expect access erchef server directly, instead of routing through
# LB.
#
internal_server "http://oc_erchef:8000"


# Related to the 'search_server' parameter, this specifies the maximum
# amout of time (in seconds) that search endpoint requests should be
# retried before giving up.  If not explicitly set, it will default to
# 65 seconds; only set it if you know that your Solr commit interval
# differs significantly from this.
maximum_search_time 65

# We're starting to break tests up into groups based on different
# criteria.  The proper API tests (the results of which are viewable
# to OPC customers) should be the only ones run by Pedant embedded in
# OPC installs.  There are other specs that help us keep track of API
# cruft that we want to come back and fix later; these shouldn't be
# viewable to customers, but we should be able to run them in
# development and CI environments.  If this parameter is missing or
# explicitly `false` only the customer-friendly tests will be run.
#
# This is mainly here for documentation purposes, since the
# command-line `oc-chef-pedant` utility ultimately determines this
# value.
include_internal false

# SSL protocol version to use for secure communications
# with the load balancer
ssl_version :TLSv1_2

# Test users.  The five users specified below are required; their
# names (:user, :non_org_user, etc.) are indicative of their role
# within the tests.  All users must have a ':name' key.  If they have
# a ':create_me' key, Pedant will create these users for you.  If you
# are using pre-existing users, you must supply a ':key_file' key,
# which should be the fully-qualified path /on the machine Pedant is
# running on/ to a private key for that user.

superuser_name 'pivotal'
superuser_key  '{{pkg.svc_config_path}}/pivotal.pem'
webui_key '{{pkg.svc_config_path}}/webui_priv.pem'
stats_user 'statsuser'

suite "api"

requestors({
             :clients => {
               :admin => {
                 :name => "pedant_admin_client_#{chef_server_uid}",
                 :create_me => true,
                 :create_knife => true,
                 :admin => true
               },
               :non_admin => {
                 :name => "pedant_client_#{chef_server_uid}",
                 :create_me => true,
                 :create_knife => true,
               },
               :bad => {
                 :name => "bad_client_#{chef_server_uid}",
                 :create_me => true,
                 :bogus => true
               }
             },

             :users => {
               # An administrator in the testing organization
               :admin => {
                 :name => "pedant_admin_user_#{chef_server_uid}",
                 :create_me => true,
                 :create_knife => true,
                 :admin => true
               },

               :non_admin => {
                 :name => "pedant_user_#{chef_server_uid}",
                 :create_me => true,
                 :create_knife => true,
                 :admin => false
               },

               # A user that is not a member of the testing organization
               :bad => {
                 :name => "pedant_nobody_#{chef_server_uid}",
                 :create_me => true,
                 :create_knife => true,
                 :associate => false
               },
             }
           })

# To facilitate testing, we have added a org creation validation tests.
# These tests run before the main Pedant tests, so their output has been
# suppressed. To make it easier to debug org creation, you can turn this
# on and get the full output
debug_org_creation false

# The behavior of these features depends on erchef configuration
search_acls?               false

old_runlists_and_search true

# Default server api version for all requests that don't specify it.
server_api_version         0

# Actions enabled. Allowing tests that require actions to be bypassed if
# actions aren't available.
actions_enabled true

# Enable/Disable tests if the required_recipe endpoint is turned on
required_recipe_enabled false

# Log HTTP Requests
# log_file "/var/log/opscode/oc-chef-pedant/http-traffic.log"

reindex_endpoint "https://127.0.0.1"

chef_pgsql_collector true
