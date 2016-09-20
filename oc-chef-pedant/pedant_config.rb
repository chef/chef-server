# This annotated Pedant configuration file details the various
# configuration settings available to you.  It is separate from the
# actual Pedant::Config class because not all settings have sane
# defaults, and not all settings are appropriate in all settings.
#
# The defaults in this file are designed to work out of the box when developing
# Chef Server 12+ using the "Dev VM" tool, but they should work in other
# contexts as long as the Chef Server is running on localhost.
#
################################################################################

# A unique identification string used to create orgs and users specific
# to a each single chef server's nodes' OS. Simply using "Process.pid"
# proved to not be unique enough when running pedant simultaneously
# on multiple nodes of the same chef server when the generated pedant
# config file could have been copied across during the setup of that
# chef server.
chef_server_uid = "private-chef_#{Process.pid}"

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
chef_server "https://#{`hostname -f`.strip}"

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
search_server "http://127.0.0.1:8983"

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
# command-line `opscode-pedant` utility ultimately determines this
# value.
include_internal false

##########################################################
# LDAP Testing, see the README.md for additional details #
##########################################################

# Set to true if you wish do LDAP testing on authenticate_user and system_recovery tests
ldap_testing false

# Fill in the following with correct values for your AD user if ldap_testing is true (directly above)
# Put :key => nil if there is no value
ldap({
       # Change this to your AD samAccountName (i.e., my login name) for your test server
       :account_name => "your_ldap_account_name",
       # Change this to your current AD password for your test server
       :account_password => "your_ldap_password!",
       # Your first name in AD
       :first_name => "Firsname",
       # Your last name in AD
       :last_name => "Lastname",
       # Your display name in AD, likely "Firstname Lastname"
       :display_name => "Firstname Lastname",
       # Your email in AD
       :email => "your@email.com",
       # Likely nil
       :city => nil,
       # Likely nil
       :country => nil,
       # Set to "linked" or "unlinked" depending on the status of your account in AD
       :status => "unlinked",
       # Set to true or false, depending on your user state in Chef itself
       :recovery_authentication_enabled => false
     })

# SSL protocol version to use for secure communications
# with the load balancer
ssl_version :TLSv1

# Test users.  The five users specified below are required; their
# names (:user, :non_org_user, etc.) are indicative of their role
# within the tests.  All users must have a ':name' key.  If they have
# a ':create_me' key, Pedant will create these users for you.  If you
# are using pre-existing users, you must supply a ':key_file' key,
# which should be the fully-qualified path /on the machine Pedant is
# running on/ to a private key for that user.

superuser_name 'pivotal'
superuser_key  '/etc/opscode/pivotal.pem'
webui_key '/etc/opscode/webui_priv.pem'

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

               # A server admin user that is not a member of the testing organization
               :server_admin => {
                 :name => "pedant_server_admin_#{chef_server_uid}",
                 :create_me => true,
                 :create_knife => true,
                 :associate => false,
                 :server_admin => true
               }
             }
           })

# To facilitate testing, we have added a org creation validation tests.
# These tests run before the main Pedant tests, so their output has been
# suppressed. To make it easier to debug org creation, you can turn this
# on and get the full output
debug_org_creation false

# To facilitate testing as we transition from Ruby to Erlang endpoint
# implementations, you can specify in your configuration which
# implementation for each endpoint is currently active on the system
# under test.  Tests should be written to fork on this value if
# necessary.  A common reason is to take into account different error
# message formatting between the two implementations.
#
old_runlists_and_search true


# Actions enabled. Allowing tests that require actions to be bypassed if
# actions aren't available.
actions_enabled true

# Log HTTP Requests
log_file "/var/log/opscode/oc-chef-pedant/http-traffic.log"
