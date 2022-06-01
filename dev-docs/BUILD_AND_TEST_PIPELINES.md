## Buildkite

**Buildkite** is a platform for running fast, secure, and scalable continuous integration pipelines on your infrastructure. Pipelines contain unit, integration, and other tests to help assess and validate your build. The first step to investigating errors is to check the build logs.

The main pipelines for this repository are:

- `[chef/chef-server:main] verify`
- `[chef/chef-server:main] omnibus/adhoc`
- `[chef/umbrella:main] chef-server`

### [chef/chef-server:main] verify

**Verify** pipeline runs all the unit tests. A verify build is automatically triggered when changes to the branch are pushed, and a pull request is linked to it. The results of an automatically-triggered verify build are linked to the pull request. If the build fails the pull request is blocked. This build can also be triggered manually.

### [chef/chef-server:main] omnibus/adhoc

**omnibus/adhoc** pipeline runs the integration tests on different builds. Integration test covers the API endpoints of the project. The pipeline creates different builds for different supported OS/environments, and pushes the builds to *jfrog* artifactory. The integration tests scripts are tested against each of the different builds created. This pipeline is automatically triggered every night to make sure that the main is always ready to ship.

### [chef/umbrella:main] chef-server

**chef-server** pipeline is for end-to-end testing and creates different builds integrating with other projects of chef. These builds are packaged to replicate the different environments in which chef-server are used by the customers. This pipeline is run on a nightly basis using the latest build from the current omnibus pipeline.
