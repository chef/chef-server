Writing Tests for Chef Server with pgTAP
========================================

Currently, the Chef Server schema tests are expressed as stored
procedures; see the `*.sql` files in the [t](../t) directory.  You can add
as many functions to these files as you like, but all _test_ functions
must start with the word "test".  Similarly, each SQL file that
contains test functions should start with "test" and have the `.sql`
extension.

The running of the tests is governed by the simple
[chef_server_schema.pg](../t/chef_server_schema.pg) file.  In general,
you shouldn't need to change this.

The [Makefile](../Makefile) has the logic responsible for loading the
test functions into the database; the logic to then select those
functions for running during the testing process is built into pgTAP,
and governed by the [chef_server_schema.pg] test file.

Take a look at the existing tests for ideas, as well as pgTAPs own
[documentation](http://pgtap.org/documentation.html).  Any test
functions not from pgTAP itself will come from the
[chef_pgtap](http://github.com/opscode/chef_pgtap) project.
