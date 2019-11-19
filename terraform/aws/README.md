# Chef Infra Server Project - Terraform

This directory contains the Terraform code used to enable Chef Infra Server developers the ability to launch ephemeral systems in different topology scenarios to enable integration test coverage.

## Pre-Requisites

### Ensure you can SSH without prompting for a passphrase

The test scenarios expect to be able to SSH directly into server instances without prompting for a passphrase.  This is most often accomplished by running an [SSH Agent](https://www.ssh.com/ssh/agent) with your private key loaded into it.  An alternative (albeit NOT recommended) approach would be to have a passphraseless SSH private key available at the default file system location (e.g. `$HOME/.ssh/id_rsa`).

### Setting up your AWS profile (for Chef Software employees only)

1. Install the [okta_aws utility](https://github.com/chef/okta_aws) - you'll use this to get credentials to the appropriate AWS account.
2. Setup `okta_aws` via the `$HOME/.okta_aws.toml` configuration file

    ***EXAMPLE:***
    ```
    [general]
    username='csnapp'
    okta_server='chef.okta.com'

    [chef-engineering]
    session_duration=43200
    ```
3. Run `okta_aws chef-engineering` to authenticate against the `chef-engineering` AWS account.

    ***NOTE:*** You MUST have an active session for terraform to be able to communicate with AWS.  The above example configures a session timeout of 12 hours for the `chef-engineering` profile.

### AWS Virtual Private Cloud (VPC)

Scenarios require that they are run against a compatible Virtual Private Cloud (VPC).

You may generate a compatible VPC using the `AWS_DEPT=Eng AWS_CONTACT=csnapp make create-vpc` command as long as your AWS region has not exceeded its VPC limit (default is 5).

***NOTE:*** **There is no automatic reaping of the VPC.  Make sure you destroy your VPC using the** `AWS_DEPT=Eng AWS_CONTACT=csnapp make destroy-vpc` **command once you are finished testing scenarios and ALL server instances have been destroyed.**

## Running a Scenario
Environment variables are used to control how the scenarios are executed and can either be passed on the command line before the `make` command or set in the shell's environment (e.g. `$HOME/.bashrc`)

### Required Environment Variables
| Environment Variable | Description | Example |
|----------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------|
| `AWS_DEPT` | Department that owns the resources should be one of: EngServ,  Operations, Eng, Training, Solutions, Sales, BD, Success or Partner | Eng |
| `AWS_CONTACT` | The primary contact for the resources, this should be the IAM username  and must be able to receive email by appending @chef.io to it (this  person can explain what/why, might not be the business owner) | csnapp |
| `AWS_SSH_KEY_ID` | The SSH key pair name within the AWS account that you have the private key for. | csnapp |
| `SCENARIO` | The name of the sub-directory within `scenarios` containing the test you'd like to run. | omnibus-tiered-fresh-install |
| `INSTALL_VERSION` | The version number of the artifact you want to install first. | 12.19.31 |
| `UPGRADE_VERSION` | The version number of the artifact you want to upgrade to. | 13.0.40+20190923060037 |

### Optional Environment Variables
| Environment Variable | Description | Example |
|-----------------------------|-----------------------------------------------------------------------------------------------|--------------------------------------------|
| `AWS_DEFAULT_PROFILE` | The name of the AWS profile used to connect to an AWS account. | chef-engineering (default) |
| `AWS_DEFAULT_REGION` | The AWS region to spawn resources within. | us-west-1 (default) |
| `AWS_DEFAULT_INSTANCE_TYPE` | The AWS instance type that determines the amount of resources server instances are allocated. | t2.medium (default) |
| `PLATFORM` | The operating system used by server instances. | rhel-6, rhel-7, rhel-8, ubuntu-16.04, ubuntu-18.04, sles-12 |
| `ENABLE_IPV6` | Use IPv6 in the chef-server.rb config and /etc/hosts | true (default) |

### Scenario Lifecycle

The test scenarios are each defined in their own terraform directory and are selected by providing the scenario name via the `SCENARIO` environment variable.

The naming convention for each scenario is `<PACKAGE_TYPE>-<TOPOLOGY>-<INSTALL_STRATEGY>` (e.g. `omnibus-tiered-fresh-install`)

An example of a typical scenario lifecycle might look like this:

1. `AWS_DEPT=Eng AWS_CONTACT=csnapp AWS_SSH_KEY_ID=csnapp SCENARIO=omnibus-tiered-fresh-install INSTALL_VERSION=12.19.31 UPGRADE_VERSION=13.0.40+20190923060037 make apply`
2. Optionally, you may SSH into the scenario instances for troubleshooting.
3. `AWS_DEPT=Eng AWS_CONTACT=csnapp AWS_SSH_KEY_ID=csnapp SCENARIO=omnibus-tiered-fresh-install INSTALL_VERSION=12.19.31 UPGRADE_VERSION=13.0.40+20190923060037 make destroy`

***NOTE:*** **There is no automatic reaping of the scenario.**

## Working with Active Scenarios

### List Active Scenarios

For terraform to track multiple concurrent scenarios it uses a concept called a `workspace`.

The `workspace` name is the combination of the following variables `SCENARIO-ENABLE_IPV6-PLATFORM` (e.g. `omnibus-tiered-fresh-install-ipv6-rhel-7`)

To get a list of the workspaces that are still active you may run the `make list-active-workspaces` command.

### Destroying Active Scenarios

To destroy all active scenarios you may run either the `make destroy-all` or `make clean` commands.

## Adding a new Scenario

1. Duplicate an existing scenario directory that is similar to the one you desire. For example, if you wanted to add a
   `omnibus-tiered-upgrade`, you could start with the `omnibus-tiered-fresh-install` scenario file.   
2. Update the `main.tf` file to reflect the scenario name as well as any additional test changes you require.
