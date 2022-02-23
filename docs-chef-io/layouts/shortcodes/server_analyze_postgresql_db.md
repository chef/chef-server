1. Run `ANALYZE` on the PostgreSQL database. This process gathers statistics for the query planner to create the most efficient query execution paths. Accurate statistics help the planner choose the most appropriate query plan, and thereby improve the speed of query processing.

    ```bash
    sudo su - opscode-pgsql
    /opt/opscode/embedded/bin/vacuumdb --all --analyze-only
    ```

   You should then see output like:

    ```bash
    vacuumdb: vacuuming database "bifrost"
    vacuumdb: vacuuming database "oc_id"
    vacuumdb: vacuuming database "opscode-pgsql"
    vacuumdb: vacuuming database "opscode_chef"
    vacuumdb: vacuuming database "postgres"
    vacuumdb: vacuuming database "template1"
    ```