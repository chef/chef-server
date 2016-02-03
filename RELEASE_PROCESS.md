# Chef Server Release Process

## Document Purpose

The purpose of this document is to describe the *current* release
process such that any member of the team can do a release.  As we
improve the automation around the release process, the document should
be updated such that it always has the exact steps required to release
Chef Server.

This document is NOT aspirational.  We have a number of automation
tools that we intend to use to improve the release release process;
however, many are not fully integrated with our process yet.. Do not
add them to this document until they are ready to be part of the
release process and can be used by any team member to perform a
release.

## Pre-requisites

In order to release, you will need the following accounts/permissions:

- Local checkouts of the chef-server and chef-web-downloads repositories
- Push access to the chef-server github repository
- Chef Software, Inc Slack account
- blog.chef.io wordpress account
- VPN account for Chef Software, Inc.
- Login for wilson.ci.opscode.us (This is linked to your github
account.)
- Access to artifactory.chef.co
- Access to delivery.chef.co
- The CHANGELOG_GITHUB_TOKEN environment variable set to a github token gathered here: https://github.com/settings/tokens/new?description=GitHub%20Changelog%20Generator%20token
- Install Github Changelog Generator: `gem install github_changelog_generator`


## THE PROCESS
### Testing the Release

Every commit to chef-server master is tested against a full pedant
run. However, upgrade testing must still be done in advance of the
release:

- [ ] Run the Chef Delivery-based automated upgrade testing
  * Checkout the `chef-server-acceptance` project from delivery.chef.co
  * Update the `chef_server_test_url-override` attributes in
    `.delivery/build/attributes/default.rb` to the URL corresponding to
    the latest package you'd like to test at artifactory.chef.co
  * Commit the change and run `delivery review`
  * When the lint and unit tests pass, approve the change and watch the
    matrix of tests run

- [ ] If this release is being made to address a specific
  high-urgency, high-severity customer issue or security issue, please
  test *specifically* that the issue in question is fixed.

If one of these tests has failed, you cannot ship a release today.
Note, no changes other than CHANGELOG/RELEASE_NOTES changes should
land on master between testing and releasing since we typically tag
HEAD of master. If something large does land on master, the release
tag you create should point specifically at the build that you tested.
The git SHA of the build you are testing can be found in
`/opt/opscode/version-manifest.json`.

### Preparing for the release

- [ ] Check that omnibus/config/projects/chef-server.rb has the
  correct version given the type of changes in this release.

- [ ] Download the previous release of Chef Server (.deb).

- [ ] Run `./dev/scripts/update-changelog.sh /path/to/deb/from/previous/step`

- [ ] Grab the section on the latest version from `NEW_CHANGELOG.md` and paste
  it at the top of `CHANGELOG.md`.

- [ ] Copy the components section from `MODIFIED_COMPONENTS_CHANGELOG.md` and
  paste into `CHANGELOG.md`

- [ ] Check RELEASE_NOTES.md to ensure that it describes the
  most important user-facing changes in the release. This file should
  form the basis of the blog post in the following step. Update as
  appropriate.

- [ ] Write a blog post for blog.chef.io. This requires a wordpress
  login. If the release contains a fix for a security vulnerability,
  you MUST tag the post as "security". The blog post should include a
  link to the downloads page (download.chef.io). Save the post as a
  draft.

### Building and Releasing the Release

- [ ] Tag the chef-server repository with the release version: `git
  tag -a VERSION_NUMBER`. The first line of the tag message should be
  the version number. The other lines are up to you. You're the boss!
  (You can leave them blank).

- [ ] Push the new tag: `git push origin master --tags`.

- [ ] Trigger a release build in Jenkins using the
  `chef-server-12-trigger-release` trigger.  Use the tag you created
  above as the GIT_REF parameter.

- [ ] Wait for the pipeline to complete.

- [ ] Use julia to promote the build: `@julia artifactory promote
  chef-server VERSION from current to stable`.  Please do this in the
  "#engineering-services" room.  Once this is done, the release is
  available to the public via the APT and YUM repositories.

- [ ] Support should already know a release is coming; however, as a
  courtesy, drop a message in the Customer Support slack channel.

- [ ] In your local checkout of the chef-web-downloads repository,
generate an update to the download pages using rake:

```
git co -b YOUR_INITIALS/release-chef-server-VERSION
export ARTIFACTORY_PASSWORD="CoolPasswordHere"
rake fetch
git add data/
# make sure all the changes are what you expect
# write a simple commit message
git commit -v
git push origin YOUR_INITIALS/release-chef-server-VERSION
```

- [ ] Open a GitHub pull request for the chef-web-downloads repository
based on the branch you just created.

- [ ] Have someone review and merge the PR.

- [ ] Once committed to master, an automated process will make the
  change to the live website; however, it might take a few minutes for
  the deploy to complete and then a few more for CDN caches to expire.

- [ ] Publish your draft blog post.

- [ ] Send an email to the chef and chef-dev mailing lists announcing
the release with a link to the blog post.

Chef Server is now released.

## Post Release

- [ ] Bump the Chef Server version number in
  `omnibus/config/projects/chef-server.rb` for development to the next
  logical version number.
