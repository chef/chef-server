Most notes are in the source files.  This just has the setup instructions.

Reconfigure using the cookbooks from this branch. You will need a build out of this branch too
because it gontains pgsql 9.6 (even though I didn't rename the software def from 9.2)

You'll need to set up the replication slot and our target DB user by hand for now:

- dvm psql opscode_chef
> SELECT * FROM pg_create_logical_replication_slot('regression_slot', 'test_decoding');
> CREATE ROLE migration_user WITH superuser replication password 'password';


Stop all all services except postgresql then create the target
DB using the current opscode_chef DB as a template:

    su opscode-pgsql
    $ /opt/opscode/embedded/bin/createdb -T opscode_chef opscode_chef_target
    $ /opt/opscode/embedded/bin/psql opscode_chef_target
    $ > GRANT ALL ON DATABASE opscode_chef_target TO "migration_user";
    $ > \q
  %

Next steps (POC):

- script or automate initial data dump
- apply tx to remote
- reindex  docat remote
- cookbook migration
- more databases - possible move migrator to standalone service?
  * bookshelf
  * oc-id (?)
  * bifrost

Longer term:
- UI/page with clicky buttons and progress!


