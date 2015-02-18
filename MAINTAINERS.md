
This file lists how the Erchef project is maintained. When making changes to the system,
this file tells you who needs to review your patch - you need a simple majority of
maintainers for the relevant subsystems to provide a :+1: on your pull request. Additionally,
you need to not receive a veto from a Lieutenant or the Project Lead.

Check out [How Chef is Maintained](https://github.com/opscode/chef-rfc/blob/master/rfc030-maintenance-policy.md#how-the-project-is-maintained) for details on
the process, how to become a maintainer, lieutenant, or the project lead.

# Project Lead

* [Stephen Delano](http://github.com/sdelano)

# Core Components

The core chef REST API server, packaging/installation,  and its primary dependencies.

* [opscode-omnibus](http://github.com/opscode/opscode-omnibus): Configuration for the Chef Server installation, and the [Omnibus](http://github.com/opscode/omnibus) project definition for building it.
* [oc_erchef](http://github.com/opscode/oc_erchef), the Erlang Chef REST API server
* [oc-chef-pedant](http://github.com/opscode/oc-chef-pedant), Chef Server tests specific to formerly closed-source features such as multi-tenancy and RBAC.
* [chef-pedant](http://github.com/opscode/chef-pedant), the base test suite and testing tools for Chef Server
* [oc_bifrost](http://github.com/opscode/oc_bifrost), the authorization service
* [bookshelf](http://github.com/opscode/bookshelf) is an S3 Compatible engine for storing cookbook data.
* [chef-mover](http://github.com/opscode/chef-mover) is the data migration orchestrator used in upgrades

### Lieutenants

* [Marc Paradise](http://github.com/marcparadise)
* [Mark Anderson](http://github.com/manderson26)

### Maintainers

* [Oliver Ferrigni](http://github.com/oferrigni)

# Supporting Components

## Knife EC Backup

[knife-ec-backup](http://github.com/opscode/knife-ec-backup) is used to ease migrations from Open Source Chef Server 11 (and below)

### Lieutenants

### Maintainers

## Knife OPC

[knife-opc](http://github.com/opscode/knife-opc) provides  administrative command-line control to the Chef Server from the console

### Lieutenants

### Maintainers
