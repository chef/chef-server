# Chef Infra Server Release Process

## Document Purpose

The purpose of this document is to describe the *current* release
process such that any member of the team can do a release. As we
improve the automation around the release process, the document should
be updated such that it always has the exact steps required to release
Chef Infra Server.

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
- Access to automate.chef.co

:warning: If it is a Friday -- do we really need that release today? :warning:

## THE PROCESS

### Update Release Notes

#### Pending Release Notes In Wiki

https://github.com/chef/chef-server/wiki/Pending-Release-Notes

### Getting the build to be released into current with a minor/major version bump

Pre-release, bump the major or minor version anytime a PR is being merged if the upcoming release is being designed around that and possibly other features.

1. Apply the label _Expeditor: Bump Version Minor_ or _Expeditor: Bump Version Major_ to your PR to bump the version number of the release candidate, as applicable.
1. DOUBLE-CHECK the labels to confirm that they are correct. A mistake here is not easy to revert.

After a commit is merged to main, expeditor automatically bumps the patch version, and a build is automatically kicked-off on the Chef / [chef/chef-server:main] omnibus/release pipeline.  After the build and tests in buildkite pass, expeditor should put the build artifact in Artifactory's current channel.  Monitor the build's progress at chef-server-notify slack channel.

3. Confirm that the omnibus build to be promoted is present in Artifactory's current channel.
One approach is to enter the following into a bash shell, where _version_ is the version number of the new release:
```
$ mixlib-install download chef-server -c current -a x86_64 -p ubuntu -l 18.04 -v <version>
Starting download https://packages.chef.io/files/current/chef-server/14.2.23/ubuntu/18.04/chef-server-core_14.2.23-1_amd64.deb
```

### Testing the Release

#### Integration Testing

Every merge to chef-server main must be built, and the various upgrade paths for the build must be tested with the full Umbrella automated integration test pipeline at https://buildkite.com/chef/chef-umbrella-main-chef-server-full (use https://buildkite.com/chef/chef-umbrella-main-chef-server for testing the 12.17.15 -> YOUR-RELEASE upgrade path).  [NOTE: Every merge to main automatically runs through adhoc, and umbrella is run nightly for all changes made that day].  The integration test run for the tag being shipped must be successful.

Any Chef Infra Server release 12.17.15 or later should be able to upgrade directly to the latest release of 14. The nightly builds test upgrades to the latest current artifact from 12.17.15 and 13.2.0.  Releases prior to 12.17.15 must perform a stepped upgrade.  See: https://docs.chef.io/server/upgrades/#upgrade-matrix

Using the step-by-step Umbrella testing process detailed below...

1. Test that the 12.17.15 release can successfully upgrade to the new release (for this, use the pipeline at https://buildkite.com/chef/chef-umbrella-main-chef-server).  This is the _12.17.15 -> YOUR-RELEASE_ upgrade path.
1. Test that the 13.2.0   release can successfully upgrade to the new release.  This is the _13.2.0 -> YOUR-RELEASE_ upgrade path.
1. Test that the latest stable release can successfully upgrade to the new release. This is the _LATEST-STABLE -> YOUR-RELEASE_ upgrade path.

Past stable releases:
https://downloads.chef.io

Umbrella Testing Step-by-Step:

1. Navigate your web browser to https://buildkite.com/chef/chef-umbrella-main-chef-server-full (or for _12.17.15 -> YOUR-RELEASE_, use https://buildkite.com/chef/chef-umbrella-main-chef-server).
1. Select 'New Build'.
1. Leave 'Branch' set to 'main'.
1. Select 'Options' to expand the 'Environment Variables' field.
1. Enter `INSTALL_VERSION=<version number>` into the 'Options | Environment Variables' field, where _version number_ is the stable channel  version number of the release you wish to test upgrading FROM (for example 12.17.15).
1. Enter `UPGRADE_VERSION=<version number>` into the 'Options | Environment Variables' field, where _version number_ is the current channel version number of the release candidate you wish to test upgrading TO (for example 14.6.32). This release candidate version number should come from the release pipeline build for the release you are doing.
1. Optionally, fill-in the 'Message' field with something descriptive.
1. Select 'Create Build'.

#### Chef Manage Testing

Currently (07/21), the Umbrella pipeline does not perform a test login to Chef Manage, so this should be done manually.  A successful test login should be verified on both IPV4 and IPV6 setups, and must use the latest stable version of Manage.  One method is to run representative AWS and Azure Umbrella scenarios from your local box to get running server instances to test.  For umbrella you should specify the Chef Manage version to install when creating the Umbrella scenario (see below), or it will pick the latest unstable version (that's generally not what you want).  For local non-Umbrella setups, `chef-server-ctl install chef-manage` will install the latest stable release of chef-manage.

Chef Manage releases:
https://downloads.chef.io/tools/manage

Typical Umbrella scenario for AWS, where _version_ is the version number of the release candidate you are testing, and _manage version_ is the latest stable version of Chef Manage:
```
$ cd umbrella/chef-server/scenarios/aws
$ okta_aws --all
Fetching credentials for: chef-engineering
...
$ make create-vpc
Initializing modules...
Downloading terraform-aws-modules/vpc/aws 2.77.0 for vpc...
...
IPV4 Scenario:
$ PLATFORM=ubuntu-18.04 INSTALL_VERSION=<version> UPGRADE_VERSION=<version> SCENARIO=standalone-fresh-install ENABLE_ADDON_PUSH_JOBS=false ENABLE_GATHER_LOGS_TEST=false ENABLE_PEDANT_TEST=false ENABLE_PSQL_TEST=false ENABLE_SMOKE_TEST=false MANAGE_VERSION=<manage_version> ENABLE_IPV6=false make apply

IPV6 Scenario:
$ PLATFORM=ubuntu-18.04 INSTALL_VERSION=<version> UPGRADE_VERSION=<version> SCENARIO=standalone-fresh-install ENABLE_ADDON_PUSH_JOBS=false ENABLE_GATHER_LOGS_TEST=false ENABLE_PEDANT_TEST=false ENABLE_PSQL_TEST=false ENABLE_SMOKE_TEST=false MANAGE_VERSION=<manage_version> ENABLE_IPV6=true make apply
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
    https://github.com/chef/umbrella/blob/main/chef-server/common/files/add_user.sh.
12. Verify that the login is successful.
13. Navigate to `http://<hostname>/id` via web browser where _hostname_ is the DNS name of the emphemeral machine obtained in the previous step.
14. Login with the same user/password as the previous login step above.
15. Select the 'signed in as <username>' link in the upper-left. Confirm that the email is grayed-out.
16. Clean-up:
```
PLATFORM=ubuntu-18.04 INSTALL_VERSION=<version> UPGRADE_VERSION=<version> SCENARIO=standalone-fresh-install ENABLE_ADDON_PUSH_JOBS=false ENABLE_GATHER_LOGS_TEST=false ENABLE_PEDANT_TEST=false ENABLE_PSQL_TEST=false ENABLE_SMOKE_TEST=false MANAGE_VERSION=<manage_version> ENABLE_IPV6=true make destroy
make destroy-vpc
```
Typical scenario for Azure, where _version_ is the version number of the release candidate you are testing:
```
$ cd umbrella/chef-server/scenarios/azure
$ ARM_DEPT=Eng ARM_CONTACT=your_login_here make create-resource-group
...
IPV4 Scenario:
$ PLATFORM=ubuntu-18.04 INSTALL_VERSION=<version> UPGRADE_VERSION=<version> SCENARIO=external-postgresql ENABLE_ADDON_PUSH_JOBS=false ENABLE_GATHER_LOGS_TEST=false ENABLE_PEDANT_TEST=false ENABLE_PSQL_TEST=false ENABLE_SMOKE_TEST=false MANAGE_VERSION=<manage_version> ENABLE_IPV6=false make apply

IPV6 Scenario:
$ PLATFORM=ubuntu-18.04 INSTALL_VERSION=<version> UPGRADE_VERSION=<version> SCENARIO=external-postgresql ENABLE_ADDON_PUSH_JOBS=false ENABLE_GATHER_LOGS_TEST=false ENABLE_PEDANT_TEST=false ENABLE_PSQL_TEST=false ENABLE_SMOKE_TEST=false MANAGE_VERSION=<manage_version> ENABLE_IPV6=true make apply
```
17. Perform the same login processes specified above for AWS.
18. Clean-up:

```
IPV4 Scenario:
PLATFORM=ubuntu-18.04 INSTALL_VERSION=<version> UPGRADE_VERSION=<version> SCENARIO=external-postgresql ENABLE_ADDON_PUSH_JOBS=false ENABLE_GATHER_LOGS_TEST=false ENABLE_PEDANT_TEST=false ENABLE_PSQL_TEST=false ENABLE_SMOKE_TEST=false MANAGE_VERSION=<manage_version> ENABLE_IPV6=false make destroy
make destroy-resource-group

IPV6 Scenario:
PLATFORM=ubuntu-18.04 INSTALL_VERSION=<version> UPGRADE_VERSION=<version> SCENARIO=external-postgresql ENABLE_ADDON_PUSH_JOBS=false ENABLE_GATHER_LOGS_TEST=false ENABLE_PEDANT_TEST=false ENABLE_PSQL_TEST=false ENABLE_SMOKE_TEST=false MANAGE_VERSION=<manage_version> ENABLE_IPV6=true make destroy
make destroy-resource-group
```

19. Any failures must be fixed before shipping a release, unless they are "known failures" or expected. A document at an unknown state of updatedness tracking known failures can be found at:
https://docs.google.com/spreadsheets/d/10LZszYuAIlrk1acy0GRhrZMd0YohLTQWmcNIdY6XuZU/edit#gid=0
Use chef.io account credentials to access it.
Note that no changes other than CHANGELOG/RELEASE_NOTES changes should land on main between testing and releasing since we typically tag HEAD of main. If something large does land on main, the release tag you create should point specifically at the build that you tested. The git SHA of the build you are testing can be found in /opt/opscode/version-manifest.json.

20. Make sure the Habitat builds for main are passing. These are kicked-off automatically on every merge.
Chef / [chef/chef-server:main] habitat/build / main
https://buildkite.com/chef/chef-chef-server-main-habitat-build

21. Test the supermarket by adding the below configuration in '/etc/opscode/chef-server.rb' and run chef-server-ctl reconfigure
```
oc_id['applications'] ||= {}
oc_id['applications']['supermarket'] = {
  'redirect_uri' => 'https://supermarket.mycompany.com/auth/chef_oauth2/callback',
}
```
The result of chef-server-ctl reconfigure should successfull for confirming supermarket is working with the latest chef-server version.
```
chef-server-ctl reconfigure
```
22. Test the chef-server version with Automate if there are any schema changes.
Follow the below document for testing the chef-server version in Automate environment.
https://github.com/chef/automate/blob/main/dev-docs/DEV_ENVIRONMENT.md

#### Special Testing

Do any special testing related to the particular release you are doing, as applicable.

### Other

Verify that the manifest displays the correct version number of the release.

http://packages.chef.io/manifests/current/chef-server/latest.json

### Building and Releasing the Release

1. Select a version from the `current` channel that you wish to promote to `stable`. Make sure that this version has gone through the upgrade testing.
2. Make sure you are part of the slack group `release-promoters`. If not added, make a request for the same in the `who-do-i-ask-about` channel.
3. Use expeditor to promote the build.  The expeditor command is of the form:

        /expeditor promote ORG/REPO:BRANCH VERSION

In practice it will look like:

        /expeditor promote chef/chef-server:main VERSION

Example:
```
/expeditor promote chef/chef-server:main 14.2.2
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

### Automate

1. Create issues in the chef-server repo and in the automate repo to update the version of Chef Infra Server in Automate. Make sure to link the issues to each other.
1. Bump the version of Automate [placeholder - instructions forthcoming].

   https://github.com/chef/chef-server/blob/main/dev-docs/AUTOMATE_DEV_ENV.md
   https://github.com/chef/automate/pull/5269

Chef Infra Server is now released.

A sample release checklist depicting a release in progress:
```
RELEASE CHECKLIST
- updated release notes                     DONE
- omnibus build in current channel          DONE
- umbrella pipeline full
    12.17.15 -> 14.6.32                     PASS [failures but no release blockers]
    https://buildkite.com/chef/chef-umbrella-main-chef-server-full/builds/44#78500beb-e0e1-4ef8-a4aa-07aefdb7a7c2
    13.2     -> 14.6.32                     PASS [failures but no release blockers]
    https://buildkite.com/chef/chef-umbrella-main-chef-server-full/builds/45#6049ca7a-9e06-4407-9cff-7c835fdae65e
    14.5.29 ->14.6.32                       PASS [failures but no release blockers]
    https://buildkite.com/chef/chef-umbrella-main-chef-server-full/builds/47#0ae8a00f-7b52-4a22-a3bf-8f4b78d94ff7
- update known failure sheet                DONE
- login to chef manage
    AWS
        ipv4                                PASS
        ipv6                                PASS
    AZURE
        ipv4                                PENDING [credentials issue]
        ipv6                                PENDING [credentials issue]
- special testing                           PASS
- check that hab builds are successful      PASS
- verify that manifest contains correct     FAIL
  release number
- build and release the release             DONE
- Create automate issues                    DONE
```
