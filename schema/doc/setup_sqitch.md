Setup Mac OS X for Using Sqitch
===============================

We use [sqitch][], a database-agnostic schema change management tool
for dealing with schema changes.  It doesn't require learning another
DSL; you get to use plain old SQL.  Some highlights include:

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
