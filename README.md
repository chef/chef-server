# Chef Server

This repository is the central repository for the Chef server.

Currently this repository does not contain any code, as the Chef server
code is spread out across several repositories.

The purpose of this repository is to server as a central place to report
Chef server issues and to outline how to contribute to the Chef Server.

If you need to file an issue against another Chef project, you can find a list of projects and where to file issues in the [community contributions section](https://docs.chef.io/community_contributions.html#issues-and-bug-reports) of the [Chef docs](https://docs.chef.io).

## Components of the Chef Server

Following is a list of key components of Chef Server. This is not yet an exhaustive list. 

### Commonly-Modified Components

* [opscode-omnibus](http://github.com/opscode/opscode-omnibus): Configuration for the Chef Server installation, and the [Omnibus](http://github.com/opscode/omnibus) project definition for building it. 
* [oc_erchef](http://github.com/opscode/oc_erchef), the REST API server. 
* [oc-chef-pedant](http://github.com/opscode/oc-chef-pedant), Chef Server tests specific to formerly closed-source features such as multi-tenancy and RBAC. 

### Less-Commonly-Modified Components

* [bookshelf](http://github.com/opscode/bookshelf), the S3-compatible engine for storing cookbook data
* [chef-mover](http://github.com/opscode/chef-mover), the data migration orchestrator used in upgrades
* [chef-pedant](http://github.com/opscode/oc-chef-pedant), the base test suite and testing tools for Chef Server
* [knife-ec-backup](http://github.com/opscode/knife-ec-backup), used to ease migrations from Open Source Chef Server 11 (and below)
* [knife-opc](http://github.com/opscode/knife-opc), used to provide administrative command-line control to the Chef Server from the console
* [oc_bifrost](http://github.com/opscode/oc_bifrost), the authorization service
* [oc-id](http://github.com/opscode/oc-id), the OAuth2 provider for extensions like Analytics or Supermarket

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
