oc_erchef - Opscode PRIVATE Chef API
====================================


This is the top-level project for the Erlang implementation of OPC's
REST API. It corresponds to the `erchef` project in OSC but includes
additional support for multitenancy and role based access controls
(authz).

This project exists to build the `oc_erchef` OTP release system and
manage the complete list of dependencies that constitute an
`oc_erchef` release.

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

#### Dependencies

The following should be installed on the development system and in your path.

+ erlang   R16B03-1
+ rebar    2.2.0
+ gecode   3.7
+ ruby     1.9.3
+ bundler  1.7.6
+ postgres 9.3.5
+ pg_ctl   9.3.5

If you haven't done so already, make sure you have initialized PostgreSQL on your machine with `initdb`:

`<path_to_postgres_executable> initdb`

#### Compiling

```
make
make bundle # this installs dep_selector
```

#### Running Tests

##### Running itests

`rebar skip_deps=true ct`

##### Running eunit tests

`rebar skip_deps=true eunit`
