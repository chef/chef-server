Setup Mac OS X for Using Sqitch
===============================

We use [sqitch][], a database-agnostic schema change management tool
for dealing with schema changes to bifrost.  It doesn't require
learning another DSL; you get to use plain old SQL.  Some highlights include:

- dependencies between changesets
- tagging of schema milestones
- deploy or revert to specific schema tags
- detailed deploy and revert event metadata stored in the database
- custom verification scripts to verify the schema at deploy time;
  nothing gets deployed if verification fails (when the database
  supports DDL in transactions, that is, like Postgres)
- support for easy reworking of existing database objects (e.g. stored
  procedures)

[sqitch]:http://sqitch.org

## Installing Sqitch

On OS X, the simplest approach is to use Homebrew:

    brew tap theory/sqitch
    brew install sqitch_pg

(Note that this command is PostgreSQL-specific, which is all we
currently care about.)

## Cheat Sheet

Sqitch comes with a CLI tool for automating some of the common tasks
surrounding the creation and management of SQL changesets.  There is
plenty of good built-in documentation, but here are some highlights.
These will be the tasks you'll be using most on a day-to-day basis as
a developer.

### Create a New Changeset

    sqitch add ${CHANGESET_NAME} [--requires ${DEPENDENCY_CHANGESET}] -n ${COMMENT}

### Tag a release

    sqitch tag ${VERSION} -n ${COMMENT}

### Deploy

    sqitch --db-name ${DATABASE} deploy --verify [--to @${VERSION}]

### Revert

    sqitch --db-name ${DATABASE} revert [--to @${VERSION}]

### Rework

    sqitch rework ${CHANGESET_TO_REWORK} -n ${COMMENT}

Use this when you need to change the contents of a previous changeset
instead of overwriting the original changeset files.

## Suggested Best Practices

### Keep Changesets Small And Focused

If you're adding a stored procedure or function, have the function
body be all that's in the changeset.  You can have other changesets
depend on it, and it'll be easier to rework the function definitions
later.

### Use Idempotent SQL Commands When Possible

For example, `CREATE TABLE IF NOT EXISTS`, `CREATE OR REPLACE
FUNCTION`, etc.  PostgreSQL doesn't yet support these clauses for all
DDL operations, but it does for many of them.  Use them when you can
(this will come into play when you need to rework a changeset.)

### Verify

You can execute any SQL in these.  The idea is to throw an error if
something isn't how you want it.  See the existing verification
scripts for some examples of how to do this for a variety of database
objects.
