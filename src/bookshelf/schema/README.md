## Schema Install Note

The complete schema is deployed as part of the chef-server installation
via the `bookshelf_database` recipe.

## Schema Upgrade Note

Upgrades are not automatically applied when chef-server upgrades are
installed/reconfigured.  In order to have a schema change applied,
you must add a partybus migration with content similar to the following:

    define_upgrade do
      run_sqitch(target: "target-tag-name", database: "bookshelf")
    end

