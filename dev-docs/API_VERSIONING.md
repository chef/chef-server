## API v0 and v1 functionality for users

### v0

#### Expectation

- The user can edit all information, including their public\_key via v0 api (which knife is currently using).
- If the user performs an update that includes a nil public key, delete the default key from the keys table for that user.

#### Backend

- Since both the users and the keys table had the record of the public\_key there were some issues when the keys are deleted and recreated. Triggers updated the information into the keys table from the users table. (Could not be done with fk because both the user and client table reference the keys table).

#### Bug

- Since every v0 API command overwrites the public\_key in the users table, triggers, and copies the incorrect key onto the correct key in the keys table. This bug is fixed with [SUSTAIN-632](https://github.com/chef/chef-server/pull/1383/files)

#### Current Status

- Currently, phase1 is in rollout.
- knife uses v0.

### v1

- The user is not allowed to edit the public\_key via the /users endpoint, it must be managed via the /keys endpoint instead.
- chef-server-ctl uses v1.

## Keys table is the only source of truth for public\_key

### Phase1(Currently implemented)

- Add a function for add\_user and update\_user that inserts only the sentinel value into the public\_key field in the users table.
- The add and update triggers in keys\_update\_trigger.sql are present to maintain compatibility with older versions of chef-server.

### Phase2(To be implemented)

#### Todo

- Delete and update triggers in keys\_update\_trigger.sql.
- Delete the public\_key column in the users (and perhaps clients) table.

Note:
- This should be easier once all the users are on Phase1 release of chef-server.
- The code then interacts with the add\_user and update\_user functions in the back.
- The upgrade path after Phase2 rollout includes upgrading to a release with Phase1 first.
