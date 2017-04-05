# Chef Server

[![Build Status](https://travis-ci.org/chef/chef-server.svg?branch=master)](https://travis-ci.org/chef/chef-server)

This repository is the central repository for the Chef server.

If you want to file an issue about Chef Server or contribute a change, you're in the right place.

If you need to file an issue against another Chef project, you can find a list of projects and where to file issues in the [community contributions section](https://docs.chef.io/community_contributions.html#issues-and-bug-reports) of the [Chef docs](https://docs.chef.io).

## Components of the Chef Server

This repository contains the core services that make up the Chef Server.

```
|-- oc-chef-pedant: A comprehensive test suite for the Chef Server API
|-- omnibus: Omnibus build configuration for the Chef Server
|-- scripts: Utility scripts
`-- src
    |-- bookshelf: S3-compatible engine for storing cookbook data
    |-- chef-mover: data migration orchestrator used in upgrades
    |-- oc-id: OAuth2 provider for extensions like Analytics or Supermarket
    |-- oc_bifrost: Chef Server's authorization service
    |-- oc_erchef: The core REST API server
    `-- opscode-expander: a service that transforms Chef data before sending it to Solr for indexing
```

## Working on the Chef Server

The quickest way to get a Chef Server development environment is to
follow the [instructions](https://github.com/chef/chef-server/blob/master/dev/README.md) in the `dev` directory.
This environment is based on Vagrant and features hot reloading of code.

## Building a Chef Server package locally:

You can build a Chef Server package locally with vagrant and test-kitchen.

```shell
cd omnibus/
make dev dev-build
```

Once the build is complete, the package should be in omnibus/pkg. By default the dev-build target will create an Ubuntu 10.04 build.

## Dependencies contained in other repositories

- [knife-ec-backup](https://www.github.com/chef/knife-ec-backup), used to ease migrations from Open Source Chef Server 11 (and below)
- [knife-opc](https://www.github.com/chef/knife-opc), used to provide administrative command-line control to the Chef Server from the console

## Major Technologies used in Chef Server

- Erlang
- PostgreSQL
- RabbitMQ
- Redis
- Solr4
- Nginx (openresty with lpeg library addition)
- Runit for service supervision

If you're looking to contribute to certain parts of the Chef server, familiarity with the following related tools is also beneficial, depending on the area.

- rebar (used for dependency management in Erlang)
- sqitch (database migrations)
- lua (routing rules in openresty)

## Contributing

For information on contributing to this project see <https://github.com/chef/chef/blob/master/CONTRIBUTING.md>

## License & Authors

**Copyright:** 2008-2017, Chef Software, Inc.

```text
Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
```
 
