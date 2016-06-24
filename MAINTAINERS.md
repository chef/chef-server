
This file lists how the Erchef project is maintained. When making changes to the system,
this file tells you who needs to review your patch - you need a simple majority of
maintainers for the relevant subsystems to provide a :+1: on your pull request. Additionally,
you need to not receive a veto from a Lieutenant or the Project Lead.

Check out [How Chef is Maintained](https://www.github.com/chef/chef-rfc/blob/master/rfc030-maintenance-policy.md#how-the-project-is-maintained) for details on
the process, how to become a maintainer, lieutenant, or the project lead.

# Project Lead

* [Stephen Delano](http://github.com/sdelano)

# Core Components

The core chef REST API server, packaging/installation,  and its primary dependencies.

* [opscode-omnibus](https://www.github.com/chef/opscode-omnibus): Configuration for the Chef Server installation, and the [Omnibus](https://www.github.com/chef/omnibus) project definition for building it.
* [oc_erchef](https://www.github.com/chef/oc_erchef), the Erlang Chef REST API server
* [oc-chef-pedant](https://www.github.com/chef/oc-chef-pedant), Chef Server tests specific to formerly closed-source features such as multi-tenancy and RBAC.
* [chef-pedant](https://www.github.com/chef/chef-pedant), the base test suite and testing tools for Chef Server
* [oc_bifrost](https://www.github.com/chef/oc_bifrost), the authorization service
* [bookshelf](https://www.github.com/chef/bookshelf) is an S3 Compatible engine for storing cookbook data.
* [chef-mover](https://www.github.com/chef/chef-mover) is the data migration orchestrator used in upgrades

### Lieutenants

* [Marc Paradise](http://github.com/marcparadise)
* [Mark Anderson](http://github.com/markan)
* [Tyler Cloke](http://github.com/tylercloke)

### Maintainers

* [Oliver Ferrigni](http://github.com/oferrigni)
* [Steven Danna](https://www.github.com/stevendanna)
* [Ryan Cragun](https://www.github.com/ryancragun)

# Supporting Components

## Knife EC Backup

[knife-ec-backup](https://www.github.com/chef/knife-ec-backup) is used to ease migrations from Open Source Chef Server 11 (and below)

### Lieutenants

* [Steven Danna](https://www.github.com/stevendanna)

### Maintainers

## Knife OPC

[knife-opc](https://www.github.com/chef/knife-opc) provides  administrative command-line control to the Chef Server from the console

### Lieutenants

* [Steven Danna](https://www.github.com/stevendanna)

### Maintainers
