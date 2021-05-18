# Chef Server Release Process

## Document Purpose

The purpose of this document is to describe the *current* release
process such that any member of the team can do a release. As we
improve the automation around the release process, the document should be updated such that it always has the exact steps required to release
Chef Server.

This document is NOT aspirational. We have a number of automation
tools that we intend to use to improve the release release process;
however, many are not fully integrated with our process yet. Do not
add them to this document until they are ready to be part of the
release process and can be used by any team member to perform a
release.

## Pre-requisites

In order to release, you will need the following accounts/permissions:

- Chef Software Inc. Slack account
- VPN account for Chef Software Inc.
- Account on [https://discourse.chef.io](https://discourse.chef.io) using your Chef email address
- Access to automate.chef.co

:warning: If it is a Friday -- do we really need that release today? :warning:

## THE PROCESS

### Update Release Notes

https://github.com/chef/chef-server/wiki/Pending-Release-Notes

### Getting the build to be released into current with a minor/major version bump

Pre-release, bump the major or minor version anytime a PR is being merged if the upcoming release is being designed around that and possibly other features.

1. Apply the label _Expeditor: Bump Version Minor_ or _Expeditor: Bump Version Major_ to your PR to bump the version number of the release candidate, as applicable.
1. DOUBLE-CHECK the labels to confirm that they are correct. A mistake here is not easy to revert.

After a commit is merged to master, expeditor automatically bumps the patch version, and a build is automatically kicked-off on the Chef / [chef/chef-server:master] omnibus/release pipeline.  After the build and tests in buildkite pass, expeditor should put the build artifact in Artifactory's current channel.  Monitor the build's progress at chef-server-notify slack channel.

3. Make sure the omnibus build to be promoted is present in Artifactory's current channel.  
One approach is to enter the following into a bash shell, where _version_ is the version number of the new release:
```
$ mixlib-install download chef-server -c current -a x86_64 -p ubuntu -l 18.04 -v <version>
Starting download https://packages.chef.io/files/current/chef-server/14.2.23/ubuntu/18.04/chef-server-core_14.2.23-1_amd64.deb
```

### Testing the Release

Every merge to chef-server master must be built, and this build must be tested with the full Umbrella automated integration test pipeline at https://buildkite.com/chef/chef-umbrella-master-chef-server-full.
The integration test run for the tag being shipped must be successful.

Any Chef Infra Server release 12.17.15 or later should be able to upgrade directly to the latest release of 14. The nightly builds test upgrades to the latest current artifact from 12.17.15 and 13.2.0.  Releases prior to 12.17.15 must perform a stepped upgrade.  See: https://docs.chef.io/server/upgrades/#upgrade-matrix

Test that the current stable release can successfully upgrade to the new release.

Past stable releases:
https://downloads.chef.io

Umbrella Testing Step-by-Step:

1. Navigate your web browser to https://buildkite.com/chef/chef-umbrella-master-chef-server
1. Select 'New Build'.
1. Leave 'Branch' set to 'master'.
1. Select 'Options' to expand the 'Environment Variables' field.
1. Enter `INSTALL_VERSION=<version number>` into the 'Options | Environment Variables' field, where _version number_ is the stable  version number of the release you wish to test upgrading FROM (for example 14.0.65).
1. Enter `UPGRADE_VERSION=<version number>` into the 'Options | Environment Variables' field, where _version number_ is the current version number of the release candidate you wish to test upgrading TO (for example 14.2.2).
1. Optionally, fill-in the 'Message' field with something descriptive.
1. Select 'Create Build'.

Currently (02/21), the Umbrella pipeline does not perform a test login to Chef Manage, so this should be done manually.  One method is to run representative AWS and Azure Umbrella scenarios locally to get running server instances to test.

Typical scenario for AWS, where _version_ is the version number of the release candidate you are testing:
```
$ cd umbrella/chef-server/scenarios/aws
$ okta_aws --all
Fetching credentials for: chef-engineering
...
$ make create-vpc
Initializing modules...
Downloading terraform-aws-modules/vpc/aws 2.77.0 for vpc...
...
$ PLATFORM=ubuntu-18.04 INSTALL_VERSION=<version> UPGRADE_VERSION=<version> SCENARIO=standalone-fresh-install ENABLE_ADDON_PUSH_JOBS=false ENABLE_GATHER_LOGS_TEST=false ENABLE_PEDANT_TEST=false ENABLE_PSQL_TEST=false ENABLE_SMOKE_TEST=false ENABLE_IPV6=true make apply
```
9. Obtain the DNS name of the ephemeral machine by observing the output of the boot-up.  A sample output is shown below:
```
null_resource.chef_server_config: Provisioning with 'remote-exec'...
null_resource.chef_server_config (remote-exec): Connecting to remote host via SSH...
null_resource.chef_server_config (remote-exec):   Host: ec2-34-212-122-231.us-west-2.compute.amazonaws.com
null_resource.chef_server_config (remote-exec):   User: ec2-user
null_resource.chef_server_config (remote-exec):   Password: false
null_resource.chef_server_config (remote-exec):   Private key: false
null_resource.chef_server_config (remote-exec):   Certificate: false
null_resource.chef_server_config (remote-exec):   SSH Agent: true
null_resource.chef_server_config (remote-exec):   Checking Host Key: false
null_resource.chef_server_config (remote-exec): Connected!
null_resource.chef_server_config (remote-exec): echo -e '
null_resource.chef_server_config (remote-exec): BEGIN INSTALL CHEF SERVER
```
10. Navigate to `http://<hostname>` via web browser where _hostname_ is the DNS name of the emphemeral machine obtained in the previous step.
11. Enter the username and password to test the login.  The username and password are stored in the following script:  
    https://github.com/chef/chef-server/blob/master/terraform/common/files/add_user.sh.  
12. Verify that the login is successful.
13. Navigate to `http://<hostname>/id` via web browser where _hostname_ is the DNS name of the emphemeral machine obtained in the previous step.
14. Login with the same user/password as the previous login step above.
15. Select the 'signed in as <username>' link in the upper-left. Confirm that the email is grayed-out.
16. Clean-up:
```
PLATFORM=ubuntu-18.04 INSTALL_VERSION=<version> UPGRADE_VERSION=<version> SCENARIO=standalone-fresh-install ENABLE_ADDON_PUSH_JOBS=false ENABLE_GATHER_LOGS_TEST=false ENABLE_PEDANT_TEST=false ENABLE_PSQL_TEST=false ENABLE_SMOKE_TEST=false ENABLE_IPV6=true make destroy
```
Typical scenario for Azure, where _version_ is the version number of the release candidate you are testing:
```
$ cd umbrella/chef-server/scenarios/azure
$ ARM_DEPT=Eng ARM_CONTACT=your_login_here make create-resource-group
...
$ PLATFORM=ubuntu-18.04 INSTALL_VERSION=<version> UPGRADE_VERSION=<version> SCENARIO=external-postgresql ENABLE_ADDON_PUSH_JOBS=false ENABLE_GATHER_LOGS_TEST=false ENABLE_PEDANT_TEST=false ENABLE_PSQL_TEST=false ENABLE_SMOKE_TEST=false ENABLE_IPV6=true make apply
```
17. Perform the same login processes specified above for AWS.
18. Clean-up:
```
PLATFORM=ubuntu-18.04 INSTALL_VERSION=<version> UPGRADE_VERSION=<version> SCENARIO=external-postgresql ENABLE_ADDON_PUSH_JOBS=false ENABLE_GATHER_LOGS_TEST=false ENABLE_PEDANT_TEST=false ENABLE_PSQL_TEST=false ENABLE_SMOKE_TEST=false ENABLE_IPV6=true make destroy
```

19. Any failures must be fixed before shipping a release, unless they are "known failures" or expected. A document at an unknown state of updatedness tracking known failures can be found at:  
https://docs.google.com/spreadsheets/d/10LZszYuAIlrk1acy0GRhrZMd0YohLTQWmcNIdY6XuZU/edit#gid=0  
Use chef.io account credentials to access it.
Note that no changes other than CHANGELOG/RELEASE_NOTES changes should land on master between testing and releasing since we typically tag HEAD of master. If something large does land on master, the release tag you create should point specifically at the build that you tested. The git SHA of the build you are testing can be found in /opt/opscode/version-manifest.json.

20. Make sure the Habitat builds for master are passing. These are kicked-off automatically on every merge. 
Chef / [chef/chef-server:master] habitat/build / master  
https://buildkite.com/chef/chef-chef-server-master-habitat-build

### Preparing for the Release

1. Check RELEASE_NOTES.md to ensure that it describes the
  most important user-facing changes in the release. This file should
  form the basis of the post to Discourse that comes in a later step.

### Building and Releasing the Release

1. Select a version from the `current` channel that you wish to promote to `stable`. Make sure that this version has gone through the upgrade testing.
1. Use expeditor to promote the build.  The expeditor command is of the form:

        /expeditor promote ORG/REPO:BRANCH VERSION

In practice it will look like:

        /expeditor promote chef/chef-server:master VERSION

Example:
```
/expeditor promote chef/chef-server:master 14.2.2
```

  Please do this in the `#chef-server` channel.  Once this is
  done, the release is available to the public via the APT and YUM
  repositories and downloads.chef.io.

### Announce the release post-promote to the following channels:
    - #a2-release-coordinate
    - #chef-server
    - #cft-announce  
Copying/pasting a discourse link to the post-promote channels should suffice.  You can find the link here.  Note that this is NOT the link to copy/paste, this is a link where you can find the link to copy/paste:  
https://discourse.chef.io/c/chef-release/9

### Create an issue for Automate

1. Create issues in the chef-server repo and in the automate repo to update the version of Chef Infra Server in Automate. Make sure to link the issues to each other.

Chef Infra Server is now released.

A sample release checklist depicting a release in progress:
```
RELEASE CHECKLIST
- updated release notes and changelog                                          YES
- version file updated                                                         YES 
- omnibus build in current channel                                             YES
- full umbrella pipeline green                                                 TESTING
  https://buildkite.com/chef/chef-umbrella-master-chef-server-full/builds/24#8b278561-477a-4548-a070-4e312a87608c
- manual login to chef-manage - AWS                                            YES
- manual login to chef-manage - azure                                          PENDING
- habitat build green                                                          PENDING
- announce pending release via slack (#a2-release-coordinate and #chef-server) PENDING
- do release                                                                   PENDING
- announce post-promote (#a2-release-coordinate, #chef-server, #cft-announce)  PENDING
```
