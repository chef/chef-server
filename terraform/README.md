# Chef Server Project - Terraform

This directory contains all the Terraform code required to spin up the chef-server.chef.co environment.

## Installation Scenarios
The acceptance environment consists of multiple different **Installation Scenarios**. These scenarios describe the
different installation and/or upgrade processes that each environment represents. The components that make up an
installation scenario include:

  * the package type (omnibus vs habitat)
  * the topology (standalone, DR/HA, etc)
  * the install strategy (fresh install, inplace upgrade, upgrade from stable channel, etc)

The naming for each installation scenario is `<PACKAGE_TYPE>-<TOPOLOGY>-<INSTALL_STRATEGY>` (e.g. `omnibus-standalone-inplace-upgrade`)

These scenario names are used in the build-cookbook and the chef-server-deploy cookbook to key off different behaviors
including grouping instances together, and applying certain configuration (if necessary).

## Exposed Chef Attributes
In addition to any attributes you specify in `chef_attributes`, the following top-level attributes are also included in
the node object for any `chef_managed_instance`.

  * chef_product_key
  * scenario_package_type
  * scenario_topology
  * scenario_install_strategy

## DNS Magic
The Ops team manages the following CNAMEs:

    chef-server-acceptance.chef.co  =>  automate-acceptance.cd.chef.co
    chef-server-union.chef.co       =>  chef-server-union.cd.chef.co
    chef-server-rehearsal.chef.co   =>  chef-server-rehearsal.cd.chef.co
    chef-server.chef.co             =>  chef-server-delivered.cd.chef.co

The `*.cd.chef.co` subdomains are managed by our Terraform scripts. Acceptance is managed as part of `terraform/acceptance` and U/R/D is managed as part of `terraform/u-r-d`.

By default, the `chef_managed_instance` module will give each instance a name corresponding with specific pieces of information. The module is smart enough to detect if product flavor and count are missing. The end result is DNS names like this:

  * <PRODUCT_KEY>-<PACKAGE_TYPE>-<TOPOLOGY>-<INSTALL_STRATEGY>-(<COUNT>-)<ENVIRONMENT>.cd.chef.co

However, these are typically very long. If you want to specify a specific subdomain, you can use the `aws_subdomain` variable. This could result in DNS names like:

  * <aws_subdomain>-(<COUNT>-)-<ENVIRONMENT>.cd.chef.co

## Adding a new Installation Scenario to Acceptance

1. Duplicate an existing scenario that is similar to the one you desire. For example, if you wanted to add a
   `omnibus-standalone-upgrade-from-current`, you could start with the `omnibus-standalone-inplace-upgrade`
   scenario file.
2. Decide if you want this scenario to be "long lived" (i.e. should the environment continue running after the
   tests have been run against it?). If this scenario can be reaped at the end of the Functional phase, add the
   automate server and job runner module definitions to the `cleanup` task in the Makefile.
3. Update the `outputs.tf` to export the FQDN for the new automate server.

## Standing up a development "acceptance" environment (for Chef Software employees only)

You can use this environment to test changes to the Terraform code and the chef-server-deploy cookbook.

1. Download and configure the [okta_aws utility](https://github.com/chef/okta_aws) - you'll use this to get credentials to the appropriate AWS account.
2. Run `okta_aws chef-cd` to authenticate against the chef-cd AWS account.
3. Allocate a `TF_ENVIRONMENT_INDEX` for yourself - reach out in the #acc-support channel to determine which number you should use.
3. Run `make plan` in the `terraform/` directory to ensure that the plan will execute accordingly.
4. Run `make apply` to stand up your environment.
5. When complete, run `make destroy` to tear down your environment. **There is no automatic reaping of these environments.**
