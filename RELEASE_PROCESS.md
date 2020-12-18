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

### Informing everyone of a pending release

- [ ] Announce your intention to drive the release to #cft-announce and #chef-server on slack.

### Getting the build to be released into current with a minor/major version bump

- Update the version file to the version to be released if minor version needs to be bumped rather than just the patch.
- Update the release notes with the version, date and context.
- Run all the tasks for update from FrequentTasks.doc
- Make sure the omnibus build is into current channel.
  (This is triggered by expeditor once the build and tests in buildkite go through ok once a commit is merged to master)
- Make sure the habitat builds are passing.
- For updating just the patch verion refer the the section below on #Preparing for the release

### Testing the Release

> This process is currently manual. There is working being done to fold this into a testing pipeline.

Every merge to chef-server master is built and this clean build is tested
with all pedant tests. We only run smoke tests for the FIPS mode. Upgrade
and addon testing must be done in advance of the release.

- [ ] Test as per the matrix from: https://docs.google.com/spreadsheets/d/1_gwxdrMnUiV8t2noi8zs1HyGivtIGMEx9KbYriIw7BY/edit#gid=536218507

- [ ] If this release is being made to address a specific
  high-urgency, high-severity customer issue or security issue, please
  *specifically* test that the issue in question is fixed.

If one of these tests has failed, you cannot ship a release until it's fixed.
Note, no changes other than CHANGELOG/RELEASE_NOTES changes should
land on master between testing and releasing since we typically tag
HEAD of master. If something large does land on master, the release
tag you create should point specifically at the build that you tested.
The git SHA of the build you are testing can be found in
`/opt/opscode/version-manifest.json`.

### Preparing for the release

- [ ] Check RELEASE_NOTES.md to ensure that it describes the
  most important user-facing changes in the release. This file should
  form the basis of the post to Discourse that comes in a later step.

### Building and Releasing the Release

- [ ] Select a version from the `current` channel that you wish to promote to `stable`. Make sure that this version has gone through the upgrade testing.
- [ ] Use expeditor to promote the build:

        /expeditor promote chef/chef-server:master VERSION

  Please do this in the `#chef-server` room. Once this is
  done, the release is available to the public via the APT and YUM
  repositories and downloads.chef.io.

- [ ] Chef employees should already know a release is coming; however, as a
  courtesy, drop a message in the #cft-announce slack channel that the release
  is coming. Provide the release number and any highlights of the release.

- [ ] Write and then publish a Discourse post on https://discourse.chef.io
  once the release is live. This post should contain a link to the downloads
  page ([https://downloads.chef.io](https://downloads.chef.io)) and its contents
  should be based on the information that was added to the RELEASE_NOTES.md file
  in an earlier step. *The post should  be published to the Chef Release
  Announcements category on https://discourse.chef.io. If it is a security
  release, it should also be published to the Chef Security Announcements
  category.* Full details on the policy of making release announcements on
  Discourse can be found on the wiki under the Engineering section ->
  Policy and Processes -> Release Announcements and Security Alerts

Chef Infra Server is now released.
