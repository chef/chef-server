# Chef Infra Server Project - Integration Test Pipeline

The [Buildkite Integration Test Pipeline](https://buildkite.com/chef/chef-chef-server-master-integration-test) provides a convenient way to run the Terraform test scenarios against the full matrix of supported distributions.

## Running the Pipeline

The simplest way to run the [Integration Test Pipeline](https://buildkite.com/chef/chef-chef-server-master-integration-test) is to rely on the defaults and just click `New Build`, optionally provide a `Message` explaining *why* you are running the pipeline, and click `Create Build`.

***NOTE:*** **Make sure to leave `Branch` set to `master` since the pipeline is defined in the `master` branch even if you are wanting to test an artifact created from your own developer branch.**

By default, the pipeline will be testing the latest `chef-server` artifact from the **current** channel while using the latest `chef-server` artifact from the **stable** channel as the initial install version for any scenario that is performing upgrade testing.

If you are wanting to test your own developer branch you must first run the [Buildkite Omnibus Adhoc Pipeline](https://buildkite.com/chef/chef-chef-server-master-omnibus-adhoc) against your developer branch and identify the `unstable` artifact version (e.g. `13.1.58+20200303212531`) found in the `Create Build Record` step.  Once you have the `unstable` artifact version you would feed it into the [Buildkite Integration Test Pipeline](https://buildkite.com/chef/chef-chef-server-master-integration-test) using the `UPGRADE_VERSION` environment variable.

See the [Overriding Defaults](Overriding-Defaults) section for more information on how you can tune the pipeline to suit your needs.

### Overriding Defaults

Environment variables are used to control how the scenarios are executed and allow the developer the ability to test artifacts that are still in unstable channels among other things.

***NOTE:*** You can find the `Environment Variables` input field under `Options` when you click `New Build`.

| Environment Variable | Description | Example |
|-----------------------------|-----------------------------------------------------------------------------------------------|--------------------------------------------|
| `INSTALL_VERSION` | The version number of the artifact you want to install first. | e.g. 12.19.31 |
| `UPGRADE_VERSION` | The version number of the artifact you want to upgrade to. | e.g. 13.1.58+20200303212531 |
| `ELASTIC_VERSION` | Version of Elasticsearch to install. | 2 or 5 with 6 being the default |
| `ENABLE_SMOKE_TEST` | Enable Chef Infra Server smoke test. | true (default) |
| `ENABLE_PEDANT_TEST` | Enable full Chef Infra Server pedant test. | true (default) |
| `ENABLE_PSQL_TEST` | Enable testing of Chef Infra Server PostgreSQL database. | true (default) |
| `ENABLE_GATHER_LOGS_TEST` | Enable testing of Chef Infra Server gathering logs. | true (default) |
| `ENABLE_ADDON_PUSH_JOBS` | Enable testing of Push Jobs addon. | true (default) |
| `ENABLE_ADDON_CHEF_MANAGE` | Enable testing of Chef Manage addon. | true (default) |
| `ENABLE_CHEF_BACKEND_DEMOTION` | Enable testing of chef-backend leadership demotion. | true (default) |

#### Example

To test an unstable `chef-server` version `13.1.58+20200303212531` upgraded from an older stable version `12.19.31` with Elasticsearch version `5` while disabling the lengthy "pedant" tests you'd feed the pipeline the following environment variables:

```
   UPGRADE_VERSION=13.1.58+20200303212531
   INSTALL_VERSION=12.19.31
   ELASTIC_VERSION=5
   ENABLE_PEDANT_TEST=false
```

## Cleaning Up Orphaned Instances

In some pipeline runs you may end up with orphaned instances which will need to be cleaned up manually.  If you have access to the `chef-cd` account you can log into the AWS EC2 web interface and find the instances (and their associated security groups) by looking for AWS resources with the prefix matching the Buildkite pipeline build number (e.g. `35`). 

If you have CLI access to the `chef-cd` AWS account you can use the `.expeditor/integration_test.pipeline.sh` script to perform the cleanup automatically by executing it like this:

```
   BUILD_NUMBER=35 ~/workspace/chef/chef-server/.expeditor/integration_test.pipeline.sh destroy-all
```