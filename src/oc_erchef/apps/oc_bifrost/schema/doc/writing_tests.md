Writing Tests with pgTAP
========================

# Basic Test Structure

```
-- Start transaction and plan the tests.
BEGIN;
SELECT no_plan();

-- Run the tests.
SELECT pass( 'My test passed, w00t!' );

-- Finish the tests and clean up.
SELECT * FROM finish();
ROLLBACK;
```

# The Details

pgTAP test suites are just plain SQL files, with optional use of
`psql` commands.  In general they can be named anything, but we follow
the convention established by the [pg_prove][] testing tool and name
them with a `.pg` extension, and store them in the `t` directory.


## Always use transaction!

Your test script should take place in a transaction; always start your
file with a `BEGIN;` statement and end with a `ROLLBACK;`.  Note that
you can use `SAVEPOINT`s in your tests, which is actually pretty
sweet.

## Have a plan

Next, you must set a "plan".  You have two options.

To explicitly set the number of tests your suite consists of (just the
tests in the current file; not overall), use `plan`:

```
SELECT plan(123);
```

To avoid having to specify the number of tests, choose `no_plan`:

```
SELECT no_plan();
```

If you opt for `plan`, the tests will be considered a success only if
exactly that number of tests run and pass.  That is, a suite with only
4 tests, all of which individually pass, will still fail overall if
you used, say, `plan(5)`.

Use `plan` to be super-strict, but realize that if you add tests, _you
must update your plan_, or you'll end up with a spuriously-failing
test suite.  For the time being, we are using `no_plan` for Authz.

## Tests are Plain SQL

Your tests themselves will just be `SELECT` queries that call any of
the various pgTAP testing functions.  You can call them individually,
or you can craft SQL statements that execute a number of tests in one
request (think of this as metaprogramming with SQL).

## Fixture Data

You can also load and otherwise modify the contents of the database
using standard `INSERT`, `UPDATE`, and `DELETE` statements (it's just
SQL, after all).  Alternatively, you can put these "fixture"
statements in a separate file and include them in the test using
`psql`'s `\i` directive.  This also allows you to share common setup
between tests.

Note that since `\i` is a `psql` directive, it is relative to the
directory that `psql` was invoked from.  As a result, if you want to
include fixture data, you'll need to refer to it from the top-level
`schema` directory, no matter how deeply nested your pgTAP test file
is.

As a consequence, `pg_prove` must be invoked from the `schema`
directory for the paths to resolve properly.

As a convention, lets store all such fixture scripts in the
[t/fixtures](../t/fixtures) directory, with an extension of `.sql`.

## Finishing

Finally, you must "finish" your test (this handles cleanup of pgTAP's
internal data and the final reporting of results).

```
SELECT * FROM finish();
```

# References

* [psql Reference](http://www.postgresql.org/docs/current/static/app-psql.html)
* [pgTAP Test Commands](http://pgtap.org/documentation.html)

[pg_prove]:http://pgtap.org/pg_prove.html
