Enterprise Chef Server PostgreSQL Schema
========================================

This repository defines additions to the
[Open Source Chef Server schema][] that will create an Enterprise Chef
database schema.

*NOTE*: This _does not_ turn an Open Source database into an
 Enterprise database.  This _does not_ provide some kind of "upgrade"
 path from the one to the other.  If you try to use it that way, your
 resulting sadness is all on you.

All instructions for the open source Chef Server schema apply for
this repository as well; perform all the setup as instructed there.

You do not need to have a local checkout of the open source schema
repository to work with this one.  The Makefile will perform a local
checkout of the open source schema, stored in the `deps` directory.
If you want to do local hacking, try setting up a symlink to your
local checkout.

Note that the specific version of the open source schema you depend on
is defined at the top of the Makefile.

[Open Source Chef Server schema]:http://github.com/opscode/chef-server-schema

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
