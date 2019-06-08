# Chef Server

[![Build Status](https://badge.buildkite.com/ccdefb69f938db51cb23f092e54030aa41608e6472cfe4aa7e.svg)](https://buildkite.com/chef/chef-chef-server-master-omnibus-adhoc)

**Umbrella Project**: [Chef Infra](https://github.com/chef/chef-oss-practices/blob/master/projects/chef-server-infra.md)

**Project State**: [Active](https://github.com/chef/chef-oss-practices/blob/master/repo-management/repo-states.md#active)

**Issues [Response Time Maximum](https://github.com/chef/chef-oss-practices/blob/master/repo-management/repo-states.md)**: 14 days

**Pull Request [Response Time Maximum](https://github.com/chef/chef-oss-practices/blob/master/repo-management/repo-states.md)**: 14 days

NOTE: we know we have a backlog, and are working through it, but this
applies for new requests.

This repository is the central repository for the Chef Server.

If you want to file an issue about Chef Server or contribute a change, you're in the right place.

If you need to file an issue against another Chef project, you can find a list of projects and where to file issues in the [community contributions section](https://docs.chef.io/community_contributions.html#issues-and-bug-reports) of the [Chef docs](https://docs.chef.io).

## Getting Help

We use GitHub issues to track bugs and feature requests. If you need help please post to our Mailing List or join the Chef Community Slack.

 * Chef Community Slack at http://community-slack.chef.io/.
 * Chef Mailing List https://discourse.chef.io/

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

## Habitized Chef Server

The following components now exist as Habitat packages and are available [here](https://bldr.habitat.sh/#/origins/chef-server/packages):
- nginx
- bookshelf
- oc_id
- oc_erchef
- oc_bifrost
- chef-server-ctl

To build the packages locally:

```shell
./habitat_pkgs_build.sh
```

A top-level `docker-compose.yml` file exists for running Chef Server from Habitized Docker images:

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

## ACC Pipeline & Chef Expeditor

Chef Software, Inc. leverages an internal Chef Automate installation, commonly referred to as ACC, to "drink our own champagne" and easily test and leverage builds of our products as they are created. The `chef/chef-server` repository, like many other Chef Software repositories, leverages an internal utility called Chef Expeditor to create a pub-sub model of actions across our various CI/CD utilities.

One of those actions is creating a change in our internal Chef Automate instance every time we successfully generate an omnibus build of Chef Server. This change allows us to easily perform a complex matrix of test suites, followed by a deployment to our internal Chef Server that we use to drive our internal infrastructure. While there is code to support this pipeline in the repository (`.delivery/**`, `terraform/**`, and `cookbooks/chef-server-deploy/**`), this repository does not depend on Chef Automate to handle pull requests.

The resources listed above that drive this pipeline, while open source, are not intended or supported for use outside of ACC as they have dependencies that are unique to Chef Software. However, please feel free to reference them for your own implementations. If you have any questions about these resources, please feel free to reach out in the [Chef Community Slack](http://community-slack.chef.io/).

## Contributing

For information on contributing to this project see <https://github.com/chef/chef/blob/master/CONTRIBUTING.md>

## License & Authors

**Copyright:** 2008-2018, Chef Software, Inc.

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
