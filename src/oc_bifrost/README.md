oc_bifrost
========

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

Bifrost?
=========

`oc_bifrost` is the Opscode Authorization API server.  It is named
after [Bifrost][], the burning rainbow bridge to Asgard in Norse mythology.

`oc_bifrost` is a complete rewrite and replacement of the old
[opscode-authz][] API server.

[Bifrost]:http://en.wikipedia.org/wiki/Bifrost
[opscode-authz]:https://github.com/opscode/opscode-authz

TODO
====

The URI's returned in response bodies are very wrong. I have marked some of the relevant tests
with `TODO URI`, but `full_uri/1` in `bifrost_wm_util.erl` is where the bad URIs are coming from.
Looks like everything URI related needs an overhaul.

Testing and Development
=======================

Everything works through the Makefile. You need to install a few prerequisite
packages before the Makefile commands with work.  See
[schema/README.md](schema/README.md) for more information.

You will need to install postgres, pgTAP, sqitch and Erlang. To find out the
correct version of Erlang to install, please see the omnibus project definition
as this is the version that is guaranteed to work with the tests.

Run `make ct` for the itests.

Run `make pedant` to run pedant tests. Note that you will need a working
postgres server running. It will assume you want to use the postgres users
`postgres`, but if you are running locally on OS X, it might be set up properly.
If you wish to use a different user (like your own username), just set `$POSTGRES_USER`
before running `make pedant`.

If you wish to reset your system/database state back to the beginning before
trying `make` again, run `make stop_test_rel` and `make destroy_test_db`.
