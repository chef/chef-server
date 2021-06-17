The goal of this document is to set up a development environment of 
automate for Chef Infra Server related work. If you encounter a process
or feature that isn’t straightforward or that took additional time for you to fully understand,
consider writing an explanation here.

## Setup

To be able to build and run Automate follow the following steps
1. Set up automate by following the docker setup in the [docs](https://github.com/chef/automate/blob/master/dev-docs/DEV_ENVIRONMENT.md#docker-setup)

2. Configure Habitat

   1. Habitat comes as part of Chef Workstation. Check if Habitat is already installed by running the command
      `hab --version`. If not installed follow the [docs](https://docs.chef.io/habitat/install_habitat/)
   2. Create an account in [Habitat](https://bldr.habitat.sh/#/sign-in) by signing in through Github.
      Create an origin there and use the same as the default origin during setup.
   3. To configure the Habitat cli run and follow the instructions: `hab cli setup`
   4. Do not connect to the on-premise builder as you have already created one in [Habitat](https://bldr.habitat.sh).
   6. Generate the [Personal Access token](https://bldr.habitat.sh/#/profile) and use the same during setup. 


3. Finally, run `hab studio enter`.

## Running Chef Infra Server in Automate dev env

Automate is a unified chef product. It is technically a habitat project. There are many different components which make up the
Automate project and each component is contained within its associated `.hart` file. Among these components, few are of Chef Infra Server like bookshelf, nginx, bifrost and erchef. The Automate project contains wrappers for the code in the Chef Infra Server project.

The following are the steps to build and deploy the components.

1. Enter studio in the root directory of the Chef Infra Server project with the command `hab studio enter`
2. Build the component `hab pkg build src/chef-server-ctl`
3. Copy the `hart` file from the results folder of Chef Infra Server project to the results folder of the Automate project.
4. Enter studio in the root directory of the Automate with the command `hab studio enter`. The automate will take the latest `hart` file copied to teh results folder.
5. To start the Chef Infra Server inside the Automate, run the command `start_chef_server`
