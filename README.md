# Chef Server

This repository is the central repository for the Chef server.

If you need to file an issue against another Chef project, you can
find a list of projects and where to file issues in the
[community contributions section](https://docs.chef.io/community_contributions.html#issues-and-bug-reports)
of the [Chef docs](https://docs.chef.io).

## Building a Chef Server package locally:

You can build a Chef Server package locally with vagrant and test-kitchen.

```
cd omnibus/
make dev dev-build
```

Once the build is complete, the package should be in omnibus/pkg.  By
default the dev-build target will create an Ubuntu 10.04 build.

## Components of the Chef Server

This repository contains the core services that make up the Chef
Server.

```
|-- oc-chef-pedant: A comprehensive test suite for the Chef Server API
|-- omnibus: Omnibus build configuration for the Chef Server
|-- scripts: Utility scripts
`-- src
    |-- bookshelf: S3-compatible engine for storing cookbook data
    |-- chef-mover: data migration orchestrator used in upgrades
    |-- chef-server-bootstrap: ruby tool to instantiate key data structures in a new chef server
    |-- oc-id: OAuth2 provider for extensions like Analytics or Supermarket
    |-- oc_bifrost: Chef Server's authorization service
    |-- oc_erchef: The core REST API server
    `-- opscode-expander: a service that transforms Chef data before sending it to Solr for indexing
```

## Dependencies contained in other repositories

* [knife-ec-backup](http://github.com/opscode/knife-ec-backup), used to ease migrations from Open Source Chef Server 11 (and below)
* [knife-opc](http://github.com/opscode/knife-opc), used to provide administrative command-line control to the Chef Server from the console

## Major Technologies used in Chef Server

* Erlang
* PostgreSQL
* RabbitMQ
* Redis
* Solr4
* Nginx (openresty with lpeg library addition)
* Runit for service supervision

If you're looking to contribute to certain parts of the Chef server, familiarity with the following related tools is also beneficial, depending on the area.

* rebar (used for dependency management in Erlang)
* keepalived (used for HA setups in the server)
* sqitch (database migrations)
* lua (routing rules in openresty)
