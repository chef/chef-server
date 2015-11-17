# Chef Server Release Notes

This document contains an overview of significant customer-facing changes
in the release. For a detailed list of changed components, refer to
[CHANGELOG.md](CHANGELOG.md).

This document contains release notes for the current release and all patches.
For prior releases, see [PRIOR\_RELEASE\_NOTES.md](PRIOR_RELEASE_NOTES.md).

## 12.3.0 (2015-11-12)

### Chef Server
  * `policy_name` and `policy_group` are now node attributes that will
    be returned with the node object, can be set via `POST` or `PUT` to
    a node, and can be used in node searches.
  * Search Improvements:
    *  Elasticsearch is now supported as a search provider.  See 'Search'
    * You can now provide your own Solr instance instead of using the one
      bundled with chef-server.
    * It is now possible for Chef Server to post updates directly to the
      search provider bypassing rabbitmq and expander. See 'Search' in the
      Configuration section below.
    * See 'Search' in the 'Configuration Changes' section
      for information about how to enable these changes.
  * The analytics queue is now monitored, and chef-server will stop sending messages
    to it when it is full.  This resolves issues where a full queue (due
    to analytics being offline) could make the chef-server
    unusable.  Status has been added to the `/_status` endpoint.

    *Important Note*: this enables the management interface of rabbitmq,
    which requires that reconfigure generate a new secure password. This
    must be distributed to nodes in the cluster via normal upgrade procedures.
  * Connection leaks that could occur under heavy and sudden load have
    been addressed.
  * The nginx status endpoint is now available and enabled by default
    for localhost access for each chef-server node via
    [`/nginx_status`](http://nginx.org/en/docs/http/ngx_http_stub_status_module.html)
    See 'nginx' in the configuration section below.
  * An issue where mismatched cases would prevent LDAP login has been
    resolved by making LDAP auth requests case-insensitive by default.  See
    'Other' in Configuration Changes for the flag to enable case-sensitivity.
  * The appearance of the default web page when navigating to the API
    server has been updated to current Chef branding.
  * Several improvements to `dvm`, the integrated development tooling.

### Security Updates

* The update to postgres 9.2.14 addresses the following:
  * CVE-2015-3165
  * CVE-2015-3167
  * CVE-2015-5288
* The update to solr 4.10.4 addresses the following:
  * CVE-2014-3628
* The oc\_id update to `jquery-rails` addresses the following:
  * CVE-2015-1840


### Bug Fixes / Closed Issues

* [459](https://github.com/chef/chef-server/issues/459) - allow
  Chef Server to work when the postgresql port has been set in
  `chef-server.rb`
* [540](https://github.com/chef/chef-server/pull/540) - default ldap to
  case insensitve, preventing login failures where case does not match
* [528](https://github.com/chef/chef-server/pull/528) - fix spurious 400s that
  clients with persistent connections could receive after requesting `/_status`
* [536](https://github.com/chef/chef-server/pull/536)  - chef-server-ctl
  ha-status no longer considers hidden services status
* [541](https://github.com/chef/chef-server/pull/541) - fix `HTTP 500` errors
  resulting from connection leaks to bifrost when under heavy load.
* [534](https://github.com/chef/chef-server/issues/534) - `chef-server-ctl backup`
  now properly exits with return code 0 unless an error occurs
* [597](https://github.com/chef/chef-server/issues/597) - add retries to
  the bootstrap-platform execute resource in case required services are still
  spinning up.
* [606](https://github.com/chef/chef-server/pull/606) - oc-id now checks
  for nil username on login, fixing failure that occurred from analytics
  login.


### API Changes

  * `GET $PREFIX/nodes/$NAME` will now include in the return fields
    `policy_name` and `policy_group`.  This change is available under
    API v0 and later.
  * `POST $PREFIX/nodes` and `PUT $PREFIX/nodes/$NAME` now accept
    `policy_name` and `policy_group` as optional fields.
  * `/_status` now includes information about the analytics queue state
    when `queue_length_monitor_enabled` is set. The additional output is
    as follows:

         [..., {"analytics_queue":
                 {"queue_at_capacity": false,
                  "last_recorded_length": 0,
                  "total_length": 0,
                  "dropped_since_last_check": 0,
                  "max_length": 0,
                  "total_dropped": 0,
                  "check_count": 0,
                  "mailbox_length": 0}}
         ]

    All values shown as 0 can be 0 or more, while `queue_at_capacity`
    will be `true` or `false`.

### Contributors

Special thanks to the following people for taking some of their own time
to contribute to Chef Server project:

* Joe DeVivo
* Shruthi Venkateswaran
* Phil Oliva
* Ryan Hass

### Chef Server Development Environment Improvements

  * sync tool talks less and gives only relevant information
  * fixes to loading of various projects that got broken when we moved
    to `rebar3` for builds.
  * dvm can now be resumed after poweroff state.
  * preliminary support for loading and developing the reporting plugin
    has been added, including an external reporting database node

### Deprecations

There are no deprecations to announce in this release.

### Configuration Changes

Below are the most relevant configuration options made available under Chef Server 12.3.0.  Unless
otherwise stated, the values shown are the default values.

For a complete listing of all available settings, see
[Chef Server Configuration](https://docs.chef.io/config_rb_server.html).

#### Search

##### External Solr

The following two configuration items have been added to support
external instances of solr or elasticsearch:

    opscode_solr4['external'] = false
    opscode_solr4['external_url'] = nil

When `external` is false, a local solr instance is configured and
used for search indexing.  When it is true, we assume that a solr
instance lives at the URL provided in `external_url`.

NOTE: In the case of an external solr, chef-server makes no
attempt to configure the correct indexes/schema.

##### Direct-to-Solr Indexing

Chef Server can now index directly to solr, bypassing rabbitmq
and opscode-expander.  To enable this, the following
configuration options have been added:

    # - 'rabbitmq' is the default and maintains the current behavior.
    # - 'batch' uses direct-to-solr indexing, batching index requests when possible
    opscode_erchef['search_queue_mode'] = "rabbitmq"

##### Elasticsearch Search Indexing

Chef Server now supports indexing to an external elasticsearch instance.
Enable this as follows:

    opscode_erchef['search_provider'] = "elasticsearch"
    opscode_solr4['external'] = true
    opscode_solr4['external_url'] = "https://PATH_TO_ELASTICSEARCH"
    opscode_erchef['search_queue_mode'] = "batch"

NOTE: Chef Server will not provision an elasticsearch server.

##### nginx

By default, each Chef Server node now has locally enabled the
`/nginx_status` endpoint on port 9999. This can be controlled with the
following settings:

    nginx['enable_stub_status'] = true
    nginx['stub_status']['listen_host'] = "127.0.0.1"
    nginx['stub_status']['listen_port'] = "9999"
    nginx['stub_status']['location'] = "/nginx_status"

##### Queue Monitoring

Chef Server now monitors the analytics queue and will stop attempting to
write to it when it is full.  Below are the relevant settings.

    # IPs permitted to access the endpoint
    nginx['stub_status']['allow_list'] = ["127.0.0.1"]

    # Enable the management interface. Required for queue length monitoring.
    rabbitmq['management_enabled'] = true

    # The maximum permitted length for the analytics queue.
    rabbitmq['analytics_max_length'] = 10000

    # How often do we check in on the state of the queue, in milliseconds?
    rabbitmq['queue_length_monitor_millis'] = 30000

    # If we can't talk to the monitor, how long do we wait before we assume
    # it is overloaded?
    rabbitmq['queue_length_monitor_timeout_millis'] = 5000

    # don't send messages to rabbitmq if it has reached it's configured max_length
    rabbitmq['drop_on_full_capacity'] = true

    # enable monitoring of the rabbit q
    rabbitmq['queue_length_monitor_enabled'] = true

    # prevent erchef from starting if queue is at capacity
    rabbitmq['prevent_erchef_startup_on_full_capacity'] = false

    # If true and capacity is reached, the chef server `/_status` endpoint
    # will report 500 when the analytics queue is at capacity.
    rabbitmq['queue_at_capacity_affects_overall_status'] = false

##### Other

*Logging*

A new configuration option for bifrost and erchef  has been provided to control
flooding of logs.  Any log messages in excess of this number per second are dropped.

    opscode_erchef['log_rotation']['max_messages_per_second'] = 1000
    oc_bifrost['log_rotation']['max_messages_per_second'] = 1000

*LDAP Case Sensitivity*

By default, Chef Server will use case insensitive authentication against
LDAP servers, as shown below.  Set the value to `true` in your
chef-server configuration to restore the previous case-sensitive behavior.

    ldap['case_sensitive_login_attribute'] = false


