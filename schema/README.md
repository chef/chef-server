Chef Server PostgreSQL Schema
=============================

This directory defines the Chef Server schema.  It depends upon the
schema present in the baseline/ directory being applied first.

*NOTE*: This _does not_ turn an Open Source database into an
 Enterprise database.  This _does not_ provide some kind of "upgrade"
 path from the one to the other.  If you try to use it that way, your
 resulting sadness is all on you.

# Modifying the Schema

The schema for the Chef Server is managed using [sqitch][], a
database-agnostic tool for managing schema changes using plain old
SQL.

* [Set up Sqitch](doc/setup_sqitch.md)
* [Using Sqitch](doc/using_sqitch.md)

# Adding Tests

We use [pgTAP][] to test both the schema and the stored procedures in
the database.

* [Developer Setup for Local Testing](doc/setup_pgtap.md)
* [Writing pgTAP Tests for Chef Server](doc/writing_tests.md)

# Bringing It All Together

You've got schema patches and tests; how do you bring it all together
and verify everything works?

```
make
```

This will create a new database, load all sqitch changesets, install
the testing framework and test functions, and finally run the tests.

If you want to do any of those steps individually, there are `make`
targets for each of them; consult the [Makefile](Makefile)

[pgTAP]:http://pgtap.org
[sqitch]:http://sqitch.org

## History

This directory was originally the enterprise-chef-server-schema at
tag 2.9.0.  All schema changes since that tag have happened here and
not in the older repository.

The baseline/ directory is the chef-server-schema directory as of
tag 1.0.4.

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

# "Monkey Patching" Test Code

Since we make use of the same test code as the open source schema, but
may have to modify those tests to work with Enterprise schema changes,
we can make use of PostgreSQL's `CREATE OR REPLACE FUNCTION` syntax to
effectively "monkey patch" the test code as appropriate.

See [t/monkey_patches.sql](t/monkey_patches.sql) and
[t/test_users_table.sql](t/test_users_table.sql) for examples of this
in practice.

This is actually the motivation for using testing functions instead of
`psql` scripts in these schema tests.

# Running the Tests

As with the open source schema, all that is required to test is:

```
make
```

(Recipient of the Seth Falcon Seal of Approval :+1:)

The targets of the [Makefile](Makefile) actually call out to
corresponding targets in the
[Open Source Makefile](https://github.com/opscode/chef-server-schema/blob/master/Makefile).
