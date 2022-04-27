# Chef Infra Server

[![Build Status](https://badge.buildkite.com/ccdefb69f938db51cb23f092e54030aa41608e6472cfe4aa7e.svg)](https://buildkite.com/chef/chef-chef-server-main-omnibus-adhoc)
[![](https://img.shields.io/badge/Release%20Policy-Cadence%20Release-brightgreen.svg)](https://github.com/chef/chef-server/blob/main/dev-docs/release_cadence.md)

**Umbrella Project**: [Chef Infra Server](https://github.com/chef/chef-oss-practices/blob/main/projects/chef-infra-server.md)

**Project State**: [Active](https://github.com/chef/chef-oss-practices/blob/main/repo-management/repo-states.md#active)

**Issues [Response Time Maximum](https://github.com/chef/chef-oss-practices/blob/main/repo-management/repo-states.md)**: 14 days

**Pull Request [Response Time Maximum](https://github.com/chef/chef-oss-practices/blob/main/repo-management/repo-states.md)**: 14 days

NOTE: we know we have a backlog, and are working through it, but this applies for new requests.

This repository is the central repository for the Chef Infra Server.

If you want to file an issue about Chef Infra Server or contribute a change, you're in the right place.

If you need to file an issue against another Chef project, you can find a list of projects and where to file issues in the [community contributions section](https://docs.chef.io/community_contributions/#issues-and-bug-reports) of the [Chef docs](https://docs.chef.io).

## Getting Help

We use GitHub issues to track bugs and feature requests. If you need help please post to our Mailing List or join the Chef Community Slack.

* Chef Community Slack at https://community-slack.chef.io/.
* Chef Mailing List https://discourse.chef.io/

## Components of the Chef Infra Server

This repository contains the core services that make up the Chef Infra Server.

```
|-- oc-chef-pedant: A comprehensive test suite for the Chef Infra Server API
|-- omnibus: Omnibus build configuration for the Chef Infra Server
|-- scripts: Utility scripts
`-- src
    |-- bookshelf: S3-compatible engine for storing cookbook data
    |-- oc-id: OAuth2 provider for extensions like Supermarket
    |-- oc_bifrost: Chef Infra Server's authorization service
    |-- oc_erchef: The core REST API server
    |-- chef-server-ctl: The Chef Infra Server's command line management utility
```

## Working on the Chef Infra Server

The quickest way to get a Chef Infra Server development environment is to
follow the [instructions](https://github.com/chef/chef-server/blob/main/dev/README.md) in the `dev` directory.
This environment is based on Vagrant and features hot reloading of code.

## Building a Chef Infra Server package locally

You can build a Chef Infra Server package locally with HashiCorp Vagrant and Test Kitchen.

```shell
cd omnibus/
make dev dev-build
```

Once the build is complete, the package should be in omnibus/pkg. By default the dev-build target will create an Ubuntu 18.04 build.

## Habitized Chef Infra Server

The following components now exist as Habitat packages and are available [here](https://bldr.habitat.sh/#/origins/chef-server/packages):

* nginx
* bookshelf
* oc_id
* oc_erchef
* oc_bifrost
* chef-server-ctl

To build the packages locally:

```shell
./habitat_pkgs_build.sh
```

A top-level `docker-compose.yml` file exists for running Chef Infra Server from Habitized Docker images:

```shell
docker-compose down && docker system prune --volumes -f && docker-compose up
```

Running pedant tests:

```shell
docker-compose exec chef-server-ctl chef-server-test
```

Running chef-server-ctl:

```shell
docker-compose exec chef-server-ctl chef-server-ctl command (subcommands)
```

## Dependencies contained in other repositories

* [knife-ec-backup](https://www.github.com/chef/knife-ec-backup), used to backup your Chef Infra Server for disaster recovery or migrations.
* [knife-opc](https://www.github.com/chef/knife-opc), used to provide administrative command-line control to the Chef Infra Server from the console

## Major Technologies used in Chef Infra Server

* Erlang
* PostgreSQL
* Redis
* Elasticsearch
* Nginx (openresty with lpeg library addition)
* Runit for service supervision

If you're looking to contribute to certain parts of the Chef Infra Server, familiarity with the following related tools is also beneficial, depending on the area.

* rebar (used for dependency management in Erlang)
* sqitch (database migrations)
* lua (routing rules in openresty)

## Chef Expeditor

The `chef/chef-server` repository, like many other Chef Software repositories, leverages an internal utility called Chef Expeditor to create a pub-sub model of actions across our various CI/CD utilities.

## Contributing

For information on contributing to this project see <https://github.com/chef/chef/blob/main/CONTRIBUTING.md>

## License & Authors

**Copyright:** 2008-2022, Chef Software, Inc.

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
