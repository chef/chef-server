## Chef Server Bootstrap

Chef Server Bootstrap is a short script to configure
the superuser on the chef server by inserting data directly into postgreSQL
erchef and bifrost databases. This solves the chicken and egg problem of
no user existing to be able to create a superuser via the API.

#### Script Usage

Note that these steps should probably never be called manually anymore.
`opscode-omnibus` should be calling this script.

`bundle install`
`./bin/bootstrap-platform CONFIG_FILE USER_CONFIG_FILE`

where `CONFIG_FILE` and `USER_CONFIG_FILE` are paths.
See the `bootstrapper-config` directory for examples of configs to pass
(or the actual configs in the `opscode-omnibus` recipe `bootstrap`).

#### Script Overview

The script is fairly simple conceptually, it is basically:

1. Read the bootstrap-configs and set up a connection to postgreSQL.
2. Create the superuser bifrost object.
  + POST to `<bifrost_url_and_port>/actors`
  using the bifrost_superuser_id as the requestor_id (should be in the config).
  + This will return the new superuser's (pivotal's) authz id to be used in the next step.
3. Create the superuser in postgreSQL.
  + Simply populate a valid hash with all the relevant fields (mostly taken from the user config).
    - Various data finagling to ensure that your hash fields match the `users` table fields in postgreSQL.
    - Use the database connection established earlier to post to postgreSQL,
    with the requestor_id being the bifrost_superuser_id, since that is the only thing that makes remote sense.
4. Create the users and organizations global containers authz objects.
  + POST to `<bifrost_url_and_port>/containers`
  + Use the superuser_authz_id (pivotal's authz_id) as the requestor_id.
  + Return the created authz_id for step 5.
5. Create the users and organizations global containers.
  + These are pretty simple objects (just a map to authz).
  + Insert simple object using the authz_id created in step 4 as the
  erchef id and authz_id, a placeholder org_id (currently 00000000000000000000000000000000)
  and last_updated_by being the superuser_authz_id (pivotal's authz_id).

You now have a functioning superuser in your chef server, enjoy :)

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
