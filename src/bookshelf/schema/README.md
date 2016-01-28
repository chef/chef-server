## Schema Install Note

The complete schema is deployed as part of the chef-server installation
via the `bookshelf_database` recipe.

## Schema Upgrade Note

Upgrades are not automatically applied when chef-server upgrades are
installed/reconfigured.  In order to have a schema change applied,
you must add a partybus migration with content similar to the following:

    define_upgrade do
      if Partybus.config.bootstrap_server
        must_be_data_master
        run_sqitch("target-tag-name", 'bookshelf')
                   'bookshelf', service: 'bookshelf')
    end

