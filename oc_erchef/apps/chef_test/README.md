## Chef Test

### Licence here

### Viewing Common Test Formatted Output

Erlang common test outputs a nice HTML formatted document for you to use. For each app in oc_erchef, open `./apps/<desired_app>/itest/ct_logs/index.html` to view the output.

### Assumptions

The code that starts the databases for you assumes that your app folder contains a folder called `./itest/common`, and that that folder contains a file called `schema.sql`. `schema.sql` can be an empty file.

#### Common Tests

You can use `ct:pal` to add to the test output. For example, `ct:pal("create query ~p~n", [chef_object:create_query(Record)]),`. This can be very useful for debugging the common tests.