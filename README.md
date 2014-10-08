# Chef Server

This repository is the central repository for the Chef server.

Currently this repository does not contain any code, as the Chef server
code is spread out across several repositories.

The purpose of this repository is to server as a central place to report
Chef server issues and to outline how to contribute to the Chef Server.

## Components of the Chef Server

The following list links to all the individual projects that make up the
Chef server.

### Commonly-Modified Components

* [bookshelf](http://github.com/opscode/bookshelf), the S3-compatible engine for storing cookbook data
* [erchef](http://github.com/opscode/erchef), the main Chef server core
* [bifrost](http://github.com/opscode/oc_bifrost), the authorization service
* [oc-id](http://github.com/opscode/oc-id), the OAuth2 provider for extensions like Analytics or Supermarket

### Less-Commonly-Modified Components

* [chef-mover](http://github.com/opscode/chef-mover), the migration orchestrator from CouchDB to PostgreSQL (used only in upgrades)
* [chef-pedant](http://github.com/opscode/oc-chef-pedant), the test suite for Chef Server
* [knife-ec-backup](http://github.com/opscode/knife-ec-backup), used to ease migrations from Open Source Chef Server 11 (and below)
* [knife-opc](http://github.com/opscode/knife-opc), used to provide administrative command-line control to the Chef Server from the console

### Packaging:

* [opscode-omnibus](http://github.com/opscode/opscode-omnibus): [Omnibus](http://github.com/opscode/omnibus) project definition for building the Chef server package.

## Major Technologies used in Chef Server

* Erlang
* PostgreSQL
* RabbitMQ
* Redis
* Solr4
* Nginx (openresty with openresty-lpeg)
* Runit for service supervision

If you're looking to contribute to certain parts of the Chef server, familiarity with the following related tools is also beneficial, depending on the area.

* rebar (used for dependency management in Erlang)
* keepalived (used for HA setups in the server)
* sqitch (database migrations)
* lua (routing rules in openresty)
