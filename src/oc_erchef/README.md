oc_erchef - Chef Server API
===========================

[![Build Status Master](https://travis-ci.org/chef/oc_erchef.svg?branch=master)](https://travis-ci.org/chef/oc_erchef)

This is the top-level project for the Erlang implementation of the
Chef Server's REST API.

This project exists to build the `oc_erchef` OTP release system and
manage the complete list of dependencies that constitute an
`oc_erchef` release.

# Chef 11

In Chef 11, the `erchef` project held the equivalent code for the Open
Source Chef Server. In Chef 12, the Enterprise and Open Source Chef
Server code-bases merged and this became the main repository.

## License

All files in the repository are licensed under the Apache 2.0 license. If any
file is missing the License header it should assume the following is attached;

```
Copyright 2014 Chef Software Inc

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
```

Release tagging and branching
-----------------------------

All dependencies are locked via `rebar.config.lock`. We update the
lock file on master and tag releases off of master.

You can update the lock file (pull in latest versions of dependencies)
as follows:

1. Decide if this should be a _patch_, _minor_, or _major_ version
   bump for oc_erchef.

2. Prepare a new release. This removes all local deps, pulls new deps
   **not using the lock file**, updates the lock file, compiles, and
   builds the OTP release, and increments the release version in
   `rel/reltool.config` based on the value of the `BUMP` environment
   variable. For example, to prepare a minor release:
   ```
   BUMP=minor make prepare_release && rebar commit-release
   ```
3. Review the commit log produced via `rebar commit-release`. It will
   show you which dependencies were added, removed, or changed. For
   changed deps, it will summarize the changes.

Note that you can perform the above steps on a feature branch of this
repo and push that through CI for testing or pull it into an
opscode-omnibus build locally (you will need to push your feature
branch).

Development
-----------

We have started to put together a development guide with some best practices and suggestions [here](https://github.com/chef/oc_erchef/blob/master/DEVELOPMENT_GUIDE.md).

#### Dependencies

The following should be installed on the development system and in your path.

+ erlang   R16B03-1
+ rebar    2.2.0
+ gecode   3.7
+ ruby     1.9.3
+ bundler  1.7.6
+ postgres 9.3.5
+ pg_ctl   9.3.5

#### Compiling

```
make
```

#### Running Tests

##### Running Integration Tests

`make ct`

You can see the output of test results by open the itest index files in your browser for the `oc_chef_authz`, `chef_db`, and `oc_chef_wm` apps:

`<path_to_repo>/apps/<app_to_view>/itest/ct_logs/index.html`

You can run individual ct tests by running:

`make ct_<full_file_name_minus_suite>`

For example, if you want to run the tests in `oc_chef_wm_server_api_version_SUITE.erl`, just run `make ct_oc_chef_wm_server_api_version`.

##### Running Unit Tests

`make test`
