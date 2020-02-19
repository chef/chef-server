# Chef Infra Server Project - Terraform

This directory contains the Terraform code used to enable Chef Infra Server developers the ability to launch ephemeral systems in different topology scenarios to enable integration test coverage.

## Pre-Requisites

### Ensure you can SSH without prompting for a passphrase

The test scenarios expect to be able to SSH directly into server instances without prompting for a passphrase.  This is most often accomplished by running an [SSH Agent](https://www.ssh.com/ssh/agent) with your private key loaded into it.  An alternative (albeit NOT recommended) approach would be to have a passphraseless SSH private key available at the default file system location (e.g. `$HOME/.ssh/id_rsa`).

### Ensure you have your SSH **public** key is saved to a file

The test scenarios require the `ARM_SSH_KEY_FILE` environment variable to be populated with the file system path to your SSH **public** key.

### Setting up your workstation to work with Azure (for Chef Software employees only)

1. Install the [Azure CLI](https://docs.microsoft.com/en-us/cli/azure/install-azure-cli).
2. Log in to Azure using `az login`
3. Create a "service principal" for your user via `az ad sp create-for-rbac --name "$USER_service_principle"`

    ***NOTE:*** Make sure you capture the client ID and secret returned by this command.  You won't be able to retrieve the client secret in the future.

### Azure Resource Manager

Scenarios require that they are run against a compatible resource manager.

You may generate a compatible resource manager using the `ARM_DEPT=Eng ARM_CONTACT=csnapp make create-resource-manager` command.

***NOTE:*** **There is no automatic reaping of the resource manager.  Make sure you destroy your resource manager using the** `ARM_DEPT=Eng ARM_CONTACT=csnapp make destroy-resource-manager` **command once you are finished testing scenarios and ALL server instances have been destroyed.**

## Running a Scenario
Environment variables are used to control how the scenarios are executed and can either be passed on the command line before the `make` command or set in the shell's environment (e.g. `$HOME/.bashrc`)

### Required Environment Variables
| Environment Variable | Description | Example |
|----------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------|
| `ARM_CLIENT_ID` | The universally unique ID associated with your service principal. | 15d3cd8b-ac2a-5319-beef-616538deadee |
| `ARM_CLIENT_SECRET` | The secret token provided by Azure when you created your service principal | - |
| `ARM_DEPT` | Department that owns the resources should be one of: EngServ,  Operations, Eng, Training, Solutions, Sales, BD, Success or Partner | Eng |
| `ARM_CONTACT` | The primary contact for the resources, this should be the IAM username  and must be able to receive email by appending @chef.io to it (this  person can explain what/why, might not be the business owner) | csnapp |
| `ARM_SSH_KEY_FILE` | The file system path to your SSH **public** key. | ~/.ssh/id_rsa.pub |
| `SCENARIO` | The name of the sub-directory within `scenarios` containing the test you'd like to run. | omnibus-external-postgresql |
| `INSTALL_VERSION` | The version number of the artifact you want to install first. | 12.19.31 |
| `UPGRADE_VERSION` | The version number of the artifact you want to upgrade to. | 13.0.40+20190923060037 |

### Optional Environment Variables
| Environment Variable | Description | Example |
|-----------------------------|-----------------------------------------------------------------------------------------------|--------------------------------------------|
| `ARM_TENANT_ID` | The name of the Azure tenant used to authenticate. | a2b2d6bc-afe1-4696-9c37-f97a7ac416d7 (default) |
| `ARM_SUBSCRIPTION_ID` | The Azure subscription used for billing. |  (default) |
| `ARM_DEFAULT_LOCATION` | Name of the Azure location to create instances in. | westus2 (default) |
| `ARM_DEFAULT_INSTANCE_TYPE` | The Azure instance type that determines the amount of resources server instances are allocated. | t2.medium (default) |
| `PLATFORM` | The operating system used by server instances. | rhel-6, rhel-7, rhel-8, ubuntu-16.04, ubuntu-18.04, sles-12 |
| `ENABLE_SMOKE_TEST` | Enable Chef Infra Server smoke test. | true (default) |
| `ENABLE_PEDANT_TEST` | Enable full Chef Infra Server pedant test. | true (default) |
| `ENABLE_PSQL_TEST` | Enable testing of Chef Infra Server PostgreSQL database. | true (default) |
| `ENABLE_GATHER_LOGS_TEST` | Enable testing of Chef Infra Server gathering logs. | true (default) |
| `ENABLE_ADDON_PUSH_JOBS` | Enable testing of Push Jobs addon. | true (default) |
| `ENABLE_ADDON_CHEF_MANAGE` | Enable testing of Chef Manage addon. | true (default) |
| `ENABLE_CHEF_BACKEND_DEMOTION` | Enable testing of chef-backend leadership demotion. | true (default) |

### Scenario Lifecycle

The test scenarios are each defined in their own terraform directory and are selected by providing the scenario name via the `SCENARIO` environment variable.

The naming convention for each scenario is `<PACKAGE_TYPE>-<TOPOLOGY>-<INSTALL_STRATEGY>` (e.g. `omnibus-external-postgresql`)

An example of a typical scenario lifecycle might look like this:

1. `ARM_DEPT=Eng ARM_CONTACT=csnapp ARM_SSH_KEY_FILE=~/.ssh/id_rsa.pub SCENARIO=omnibus-external-postgresql INSTALL_VERSION=12.19.31 UPGRADE_VERSION=13.0.40+20190923060037 make apply`
2. Optionally, you may SSH into the scenario instances for troubleshooting.
3. `ARM_DEPT=Eng ARM_CONTACT=csnapp ARM_SSH_KEY_FILE=~/.ssh/id_rsa.pub SCENARIO=omnibus-external-postgresql INSTALL_VERSION=12.19.31 UPGRADE_VERSION=13.0.40+20190923060037 make destroy`

***NOTE:*** **There is no automatic reaping of the scenario.**

## Working with Active Scenarios

### List Active Scenarios

For terraform to track multiple concurrent scenarios it uses a concept called a `workspace`.

The `workspace` name is the combination of the following variables `SCENARIO-ENABLE_IPV6-PLATFORM` (e.g. `omnibus-external-postgresql-ipv6-rhel-7`)

To get a list of the workspaces that are still active you may run the `make list-active-workspaces` command.

### Destroying Active Scenarios

To destroy all active scenarios you may run either the `make destroy-all` or `make clean` commands.

## Adding a new Scenario

1. Duplicate an existing scenario directory that is similar to the one you desire. For example, if you wanted to add a
   `omnibus-tiered-upgrade-from-stable`, you could start with the `omnibus-external-postgresql` scenario file.   
2. Update the `main.tf` file to reflect the scenario name as well as any additional test changes you require.
