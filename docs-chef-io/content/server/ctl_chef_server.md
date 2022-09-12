+++
title = "chef-server-ctl (executable)"
draft = false

gh_repo = "chef-server"

aliases = ["/ctl_chef_server.html", "/ctl_chef_server/"]

[menu]
  [menu.server]
    title = "chef-server-ctl"
    identifier = "server/reference/chef-server-ctl"
    parent = "server/reference"
    weight = 10
+++

{{% chef-server/ctl_chef_server_summary %}}

## Backup / Restore

Use the following commands to manage backups of Chef Infra Server data,
and then to restore those backups.

### backup

{{% chef-server/ctl_chef_server_backup %}}

**Options**

{{% chef-server/ctl_chef_server_backup_options %}}

**Syntax**

{{% chef-server/ctl_chef_server_backup_syntax %}}

### restore

{{% chef-server/ctl_chef_server_restore %}}

**Options**

{{% chef-server/ctl_chef_server_restore_options %}}

**Syntax**

{{% chef-server/ctl_chef_server_restore_syntax %}}

**Examples**

```bash
chef-server-ctl restore /path/to/tar/archive.tgz
```

## cleanse

The `cleanse` subcommand is used to re-set the Chef Infra Server to the
state it was in prior to the first time the `reconfigure` subcommand is
run. This command will destroy all data, configuration files, and logs.
The software that was put on-disk by the package installation will
remain; re-run `chef-server-ctl reconfigure` to recreate the default
data and configuration files.

**Options**

This subcommand has the following options:

`--with-external`

:   Use to specify that Chef Infra Server data on an external PostgreSQL
    database should be removed.

**Syntax**

This subcommand has the following syntax:

```bash
chef-server-ctl cleanse
```

## gather-logs

The `gather-logs` subcommand is used to gather the Chef Infra Server log
files into a tarball that contains all of the important log files and
system information.

This subcommand has the following syntax:

```bash
chef-server-ctl gather-logs
```

## help

The `help` subcommand is used to print a list of all available
chef-server-ctl commands.

This subcommand has the following syntax:

```bash
chef-server-ctl help
```

## install

The `install` subcommand is used to install premium features of the Chef
Infra Server: Chef management console and Chef Infra Client run
reporting, high availability configurations, and Chef
Infra Server replication.

{{< warning >}}

The `chef-server-ctl install` command no longer works in the 12.5 (and
earlier) versions of the Chef Infra Server due to a change in how
packages are downloaded from Chef.

{{< /warning >}}

**Syntax**

This subcommand has the following syntax:

```bash
chef-server-ctl install name_of_addon (options)
```

where `name_of_addon` represents the command line value associated with
the add-on or premium feature.

**Options**

This subcommand has the following options:

`--path PATH`

:   Use to specify the location of a package. This option is not
    required when packages are downloaded from
    <https://packages.chef.io/>.

### Use Downloads

{{% chef-server/ctl_chef_server_install_features_download %}}

### Use Local Packages

{{% chef-server/ctl_chef_server_install_features_manual %}}

## Key Rotation

Use the following commands to manage public and private key rotation for
users and clients.

### add-client-key

Use the `add-client-key` subcommand to add a client key.

**Syntax**

This subcommand has the following syntax:

```bash
chef-server-ctl add-client-key ORG_NAME CLIENT_NAME [--public-key-path PATH] [--expiration-date DATE] [--key-name NAME]
```

{{< warning >}}

All options for this subcommand must follow all arguments.

{{< /warning >}}

**Options**

This subcommand has the following options:

`CLIENT_NAME`

:   The name of the client that you wish to add a key for.

`-e DATE` `--expiration-date DATE`

:   An ISO 8601 formatted string: `YYYY-MM-DDTHH:MM:SSZ`. For example:
    `2013-12-24T21:00:00Z`. If not passed, expiration will default to
    infinity.

`-k NAME` `--key-name NAME`

:   String defining the name of your new key for this client. If not
    passed, it will default to the fingerprint of the public key.

`ORG_NAME`

:   The short name for the organization to which the client belongs.

`-p PATH` `--public-key-path PATH`

:   The location to a file containing valid PKCS\#1 public key to be
    added. If not passed, then the server will generate a new one for
    you and return the private key to STDOUT.

### add-user-key

Use the `add-user-key` subcommand to add a user key.

**Syntax**

This subcommand has the following syntax:

```bash
chef-server-ctl add-user-key USER_NAME [--public-key-path PATH] [--expiration-date DATE] [--key-name NAME]
```

{{< warning >}}

All options for this subcommand must follow all arguments.

{{< /warning >}}

**Options**

This subcommand has the following options:

`-e DATE` `--expiration-date DATE`

:   An ISO 8601 formatted string: `YYYY-MM-DDTHH:MM:SSZ`. For example:
    `2013-12-24T21:00:00Z`. If not passed, expiration will default to
    infinity.

`-k NAME` `--key-name NAME`

:   String defining the name of your new key for this user. If not
    passed, it will default to the fingerprint of the public key.

`-p PATH` `--public-key-path PATH`

:   The location to a file containing valid PKCS\#1 public key to be
    added. If not passed, then the server will generate a new one for
    you and return the private key to STDOUT.

`USER_NAME`

:   The user name for the user for which a key is added.

### delete-client-key

Use the `delete-client-key` subcommand to delete a client key.

**Syntax**

This subcommand has the following syntax:

```bash
chef-server-ctl delete-client-key ORG_NAME CLIENT_NAME KEY_NAME
```

**Options**

This subcommand has the following arguments:

`ORG_NAME`

:   The short name for the organization to which the client belongs.

`CLIENT_NAME`

:   The name of the client.

`KEY_NAME`

:   The unique name to be assigned to the key you wish to delete.

### delete-user-key

Use the `delete-user-key` subcommand to delete a user key.

**Syntax**

This subcommand has the following syntax:

```bash
chef-server-ctl delete-user-key USER_NAME KEY_NAME
```

{{< warning >}}

The parameters for this subcommand must be in the order specified above.

{{< /warning >}}

**Options**

This subcommand has the following arguments:

`USER_NAME`

:   The user name.

`KEY_NAME`

:   The unique name to be assigned to the key you wish to delete.

### list-client-keys

Use the `list-client-keys` subcommand to list client keys.

**Syntax**

This subcommand has the following syntax:

```bash
chef-server-ctl list-client-keys ORG_NAME CLIENT_NAME [--verbose]
```

{{< warning >}}

All options for this subcommand must follow all arguments.

{{< /warning >}}

**Options**

This subcommand has the following options:

`CLIENT_NAME`

:   The name of the client.

`ORG_NAME`

:   The short name for the organization to which the client belongs.

`--verbose`

:   Use to show the full public key strings in command output.

### list-user-keys

Use the `list-user-keys` subcommand to list client keys.

**Syntax**

This subcommand has the following syntax:

```bash
chef-server-ctl list-user-keys USER_NAME [--verbose]
```

{{< warning >}}

All options for this subcommand must follow all arguments.

{{< /warning >}}

**Options**

This subcommand has the following options:

`USER_NAME`

:   The user name you wish to list keys for.

`--verbose`

:   Use to show the full public key strings in command output.

**Example**

To view a list of user keys (including public key output):

```bash
chef-server-ctl list-user-keys applejack --verbose
```

Returns:

```bash
2 total key(s) found for user applejack

key_name: test-key
expires_at: Infinity
public_key:
-----BEGIN PUBLIC KEY-----
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA4q9Dh+bwJSjhU/VI4Y8s
9WsbIPfpmBpoZoZVPL7V6JDfIaPUkdcSdZpynhRLhQwv9ScTFh65JwxC7wNhVspB
4bKZeW6vugNGwCyBIemMfxMlpKZQDOc5dnBiRMMOgXSIimeiFtL+NmMXnGBBHDaE
b+XXI8oCZRx5MTnzEs90mkaCRSIUlWxOUFzZvnv4jBrhWsd/yBM/h7YmVfmwVAjL
VST0QG4MnbCjNtbzToMj55NAGwSdKHCzvvpWYkd62ZOquY9f2UZKxYCX0bFPNVQM
EvBQGdNG39XYSEeF4LneYQKPHEZDdqe7TZdVE8ooU/syxlZgADtvkqEoc4zp1Im3
2wIDAQAB
-----END PUBLIC KEY-----

key_name: default
expires_at: Infinity
public_key:
-----BEGIN PUBLIC KEY-----
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA4q9Dh+bwJSjhU/VI4Y8s
9WsbIPfpmBpoZoZVPL7V6JDfIaPUkdcSdZpynhRLhQwv9ScTFh65JwxC7wNhVspB
4bKZeW6vugNGwCyBIemMfxMlpKZQDOc5dnBiRMMOgXSIimeiFtL+NmMXnGBBHDaE
b+XXI8oCZRx5MTnzEs90mkaCRSIUlWxOUFzZvnv4jBrhWsd/yBM/h7YmVfmwVAjL
VST0QG4MnbCjNtbzToMj55NAGwSdKHCzvvpWYkd62ZOquY9f2UZKxYCX0bFPNVQM
EvBQGdNG39XYSEeF4LneYQKPHEZDdqe7TZdVE8ooU/syxlZgADtvkqEoc4zp1Im3
2wIDAQAB
-----END PUBLIC KEY-----
```

## Maintenance Mode

Starting with Chef Infra Server 14.6.32, you can now place your Chef Infra Server into maintenance mode to block requests from Chef Infra Clients or tools like knife. Maintenance mode also allows you to specify allowed IP addresses for testing your Infra Server without allowing access to all clients.

**Syntax**

This subcommand has the following syntax:

* Turn on maintenance mode: `chef-server-ctl maintenance on`
* Turn off maintenance mode: `chef-server-ctl maintenance off`
* Add an allowed IP address: `chef-server-ctl maintenance -a IP_ADDRESS`
* Remove an allowed IP address: `chef-server-ctl maintenance -r IP_ADDRESS`
* List all allowed IP address: `chef-server-ctl maintenance -l`

## Secrets Management

Use the following commands to manage and rotate shared secrets and
service credentials. The secrets file used for storing these is located
at `/etc/opscode/private-chef-secrets.json` on your Chef Infra Server.
It should be owned and readable only by `root`.

### set-secret

The `set-secret` subcommand allows storing shared secrets and service
credentials. Only secrets known to Chef Infra Server can be stored.

**Syntax**

This subcommand has the following syntax:

```bash
chef-server-ctl set-secret GROUP NAME
```

There are various ways to pass the secret to this command:

1.  as a third argument:

    ```bash
    chef-server-ctl set-secret ldap bind_password secretpassword
    ```

2.  via an environment variable:

    ```bash
    export LDAP.BIND_PASSWORD="secretpassword"
    chef-server-ctl set-secret ldap bind_password
    ```

3.  via an interactive prompt:

    ```bash
    chef-server-ctl set-secret ldap bind_password
    Enter ldap bind_password:    (no terminal output)
    Re-enter ldap bind_password: (no terminal output)
    ```

**Options**

This subcommand has the following options:

`--with-restart`

:   If any services depend on the secret being changed, attempt to
    restart them after changing the secret. Added in Chef Infra Server
    12.16.2.

### remove-secret

The `remove-secret` subcommand allows removing a stored shared secret
and service credential.

**Syntax**

This subcommand has the following syntax:

```bash
chef-server-ctl remove-secret GROUP NAME
```

**Example**

```bash
chef-server-ctl remove-secret ldap bind_password
```

### show-secret

The `show-secret` subcommand allows viewing a stored shared secret and
service credential.

**Syntax**

This subcommand has the following syntax:

```bash
chef-server-ctl show-secret GROUP NAME
```

### set-db-superuser-password

The `set-db-superuser-password` subcommand allows storing the database
superuser password.

**Syntax**

This subcommand has the following syntax:

```bash
chef-server-ctl set-db-superuser-password
```

Similar to `set-secret`, the superuser password can also be provided via
the environment variable `DB_PASSWORD`.

### set-actions-password

The `set-actions-password` subcommand allows storing the RabbitMQ
Actions password.

**Syntax**

This subcommand has the following syntax:

```bash
chef-server-ctl set-actions-password
```

Similar to `set-secret`, the action password can also be provided via
the environment variable `ACTIONS_PASSWORD`.

### oc-id-show-app

The `oc-id-show-app` subcommand allows for retrieving the client ID and
client secret for applications known to **oc-id**. Note that with
`insecure_addon_compat` [disabled]({{< relref "server_security/#chef-infra-server-credentials-management" >}}),
this data will no longer be written to `/etc/opscode/oc-id-applications/APP.json`.

**Syntax**

This subcommand has the following syntax:

```bash
chef-server-ctl oc-id-show-app APP
```

**Example**

```bash
chef-server-ctl oc-id-show-app supermarket
{
  "name": "supermarket",
  "uid": "0bad0f2eb04e935718e081fb71asdfec3681c81acb9968a8e1e32451d08b",
  "secret": "17cf1141cc971a10ce307611beda7ffadstr4f1bc98d9f9ca76b9b127879",
  "redirect_uri": "https://supermarket.mycompany.com/auth/chef_oauth2/callback"
}
```

### require-credential-rotation

The `require-credential-rotation` subcommand takes the Chef Infra Server
offline and requires a complete service credential rotation before the
Chef Infra Server(s) in your cluster can restart again. Run
`rotate-shared-secrets` to create a new shared secret, salt, and
generate the new service credentials. Then copy the secrets file to each
Chef Infra Server and run `sudo chef-server-ctl reconfigure` on each
server to complete the rotation process.

{{< note >}}

Credential rotation does not rotate the pivotal, user, or client keys,
or remove any Chef Infra Server policy or cookbooks that have been
uploaded.

{{< /note >}}

**Syntax**

This subcommand has the following syntax:

```bash
chef-server-ctl require-credential-rotation (options)
```

**Options**

This subcommand has the following options:

`-y, --yes`

:   Bypass a prompt in the terminal and agree that you want to disable
    the Chef Infra Server, and require credential rotation.

### rotate-all-credentials

The `rotate-all-credentials` subcommand generates new credential values
for all service credentials by incrementing the credential version
number and creating a new hash value. You can choose whether to copy the
updated secrets file to each node in the cluster and reconfiguring or by
running this subcommand on all the nodes.

**Syntax**

This subcommand has the following syntax:

```bash
chef-server-ctl rotate-all-credentials
```

### rotate-credentials

The `rotate-credentials` subcommand generates new credential values for
all credentials for a given service by incrementing the value and
creating a new hash value. You can choose whether to copy the updated
secrets file to each node in the cluster and reconfiguring or by running
this subcommand for that specific service on all the nodes.

**Syntax**

This subcommand has the following syntax:

```bash
chef-server-ctl rotate-credentials SERVICE_NAME
```

### rotate-shared-secrets

The `rotate-shared-secrets` subcommand creates a new shared secret and
salt, in addition to generating new service credentials. It also resets
the `credential_version` number for the services to 0. After you have
run this subcommand, a new shared secret has been created, so you must
copy the secrets file to each Chef Infra Server and run
`sudo chef-server-ctl reconfigure` on them to complete the rotation
process.

**Syntax**

This subcommand has the following syntax:

```bash
chef-server-ctl rotate-shared-secrets
```

### show-service-credentials

The `show-service-credentials` subcommand shows all of the service
credentials for services running on the local Chef Infra Server.

**Syntax**

This subcommand has the following syntax:

```bash
chef-server-ctl show-service-credentials
```

### cleanup-bifrost

The `cleanup-bifrost` subcommand removes unused authorization IDs for actors and objects
from the authorization database (called bifrost).

These unused actor authorization IDs
can accumulate on long-running Chef Infra Servers as a result of failed actor creation
requests. For most users, the unused authorization actors do
not substantially affect the performance of Chef Infra Server; however
in certain situations it can be helpful to clean them up.

The unused object authorization IDs can accumulate while cookbooks are uploading.
During the cookbook upload, Infra Server creates a sandbox atomic transaction to verify that partial cookbook content
is not uploaded to the server. This issue is fixed in Chef Infra Server 14.16.

**Syntax**

This subcommand has the following syntax:

```bash
chef-server-ctl cleanup-bifrost OPTIONS
```

**Options**

This subcommand has the following options:

`--estimate-only`

:   Provides an estimate of the number of unused objects that will be
    deleted, without deleting anything.

`--wait-time SECONDS`

:   The number of seconds to wait for in-flight requests to complete.
    Only decrease this value if you are running the command when the
    Chef Infra Server is not taking requests.

`--force-cleanup`

:   Removes internal tracking tables used during the cleanup process.
    Manual cleanup of these tables is only required if the cleanup
    command is killed unexpectedly.

`--batch-size`

:   The number of orphaned authorization actors to delete at a time.

## Manage Organizations

{{% chef-server/ctl_chef_server_org %}}

### org-create

{{% chef-server/ctl_chef_server_org_create %}}

**Syntax**

{{% chef-server/ctl_chef_server_org_create_syntax %}}

**Options**

{{% chef-server/ctl_chef_server_org_create_options %}}

**Examples**

```bash
chef-server-ctl org-create prod Production
```

```bash
chef-server-ctl org-create staging Staging -a chef-admin
```

```bash
chef-server-ctl org-create dev Development -f /tmp/id-dev.key
```

```bash
chef-server-ctl org-create dev Development --association_user grantmc
```

### org-delete

{{% chef-server/ctl_chef_server_org_delete %}}

**Syntax**

{{% chef-server/ctl_chef_server_org_delete_syntax %}}

**Examples**

```bash
chef-server-ctl org-delete infra-testing-20140909
```

```bash
chef-server-ctl org-delete pedant-testing-org
```

### org-list

{{% chef-server/ctl_chef_server_org_list %}}

**Syntax**

{{% chef-server/ctl_chef_server_org_list_syntax %}}

**Options**

{{% chef-server/ctl_chef_server_org_list_options %}}

### org-show

{{% chef-server/ctl_chef_server_org_show %}}

**Syntax**

{{% chef-server/ctl_chef_server_org_show_syntax %}}

### org-user-add

{{% chef-server/ctl_chef_server_org_user_add %}}

**Syntax**

{{% chef-server/ctl_chef_server_org_user_add_syntax %}}

**Options**

{{% chef-server/ctl_chef_server_org_user_add_options %}}

**Examples**

```bash
chef-server-ctl org-user-add prod john_smith
```

```bash
chef-server-ctl org-user-add preprod testmaster
```

```bash
chef-server-ctl org-user-add dev grantmc --admin
```

### org-user-remove

{{% chef-server/ctl_chef_server_org_user_remove %}}

{{< warning >}}

{{% knife_edit_admin_users %}}

{{< /warning >}}

**Syntax**

{{% chef-server/ctl_chef_server_org_user_remove_syntax %}}

**Options**

This subcommand has the following options:

`--force`

:   Force the removal of a user from the organization's `admins` and
    `billing-admins` groups.

**Examples**

```bash
chef-server-ctl org-user-remove prod john_smith
```

```bash
chef-server-ctl org-user-remove prod testmaster
```

```bash
chef-server-ctl org-user-remove grantmc --force
```

## password

The `password` subcommand is used to change a user's password. When
Active Directory or LDAP is enabled, this command enables (or disables)
the system recovery password for that user. For example:

This subcommand has the following syntax:

```bash
chef-server-ctl password USERNAME
```

This subcommand has the following options:

`--disable`

:   Use this option to disable a user's system recovery password.

**Examples**

For example, to change a user's password, enter:

```bash
chef-server-ctl password adamjacobs
```

and then enter the password and confirm it:

```bash
Enter the new password:  ******
Enter the new password again:  ******
```

to return:

```bash
Password for adamjacobs successfully set.
```

To disable a system recovery password:

```bash
chef-server-ctl password adamjacobs --disable
```

to return:

```bash
Password for adamjacobs successfully disabled for System Recovery.
```

## psql

The `psql` subcommand is used to log into the PostgreSQL database
associated with the named service. This subcommand:

-   Uses `psql` (the interactive terminal for PostgreSQL)
-   Has read-only access by default
-   Is the recommended way to interact with any PostgreSQL database that
    is part of the Chef Infra Server
-   Automatically handles authentication

**Syntax**

This subcommand has the following syntax:

```bash
chef-server-ctl psql SERVICE_NAME (options)
```

**Options**

This subcommand has the following options:

`--write`

:   Use to enable write access to the PostgreSQL database.

## reconfigure

The `reconfigure` subcommand is used when changes are made to the
chef-server.rb file to reconfigure the server. When changes are made to
the chef-server.rb file, they will not be applied to the Chef Infra
Server configuration until after this command is run. This subcommand
will also restart any services for which the `service_name['enabled']`
setting is set to `true`.

This subcommand has the following syntax:

```bash
chef-server-ctl reconfigure
```

## reindex

The `reindex` subcommand is used to reload Chef Infra Server data from
PostgreSQL to Elasticsearch.

This subcommand has the following syntax:

```bash
chef-server-ctl reindex
```

{{< note >}}
Chef Infra Server versions earlier than 14.5 configured with `nginx['enable_non_ssl'] = false` and `fips = true` require `export CSC_LB_URL=https://127.0.0.1` to run the command `chef-server-ctl reindex <options>`
{{< /note >}}

**Options**

This subcommand has the following options:

`-a`, `--all-orgs`

:   Use to reindex all organizations on the Chef Infra Server. This
    option will override any organization specified as part of the
    command, i.e. `chef-server-ctl reindex ORG_NAME -a` will reindex all
    organizations and not just the specified organization.

`-d`, `--disable-api`

:   Use to disable the Chef Infra Server API to prevent writes during
    reindexing.

`-t`, `--with-timing`

:   Use to print timing information for the reindex processes.

`-w`, `--wait`

:   Use to wait for the reindexing queue to clear before exiting. This
    option only works when run on a standalone Chef Infra Server or on a
    primary backend Chef server within a legacy tier.

{{< note >}}
If `knife search` does not return the expected results and data is present in the Chef Infra Server after reindex, then verify the search index configuration with the command:

```bash
curl -XGET http://127.0.0.1:9200/_all/_settings`
```

Set the `read_only_allow_delete` to false. Use this command to reset the search index configuration :

```bash
curl -XPUT -H "Content-Type: application/json" http://127.0.0.1:9200/_all/_settings -d '{"index.blocks.read_only_allow_delete": null}'
```

{{< /note >}}

## Server Admins

{{% chef-server/server_rbac_server_admins %}}

### Scenario

{{% chef-server/server_rbac_server_admins_scenario %}}

#### Superuser Accounts

{{% chef-server/server_rbac_server_admins_superusers %}}

### Manage server-admins Group

{{% chef-server/ctl_chef_server_server_admin %}}

#### Add Members

{{% chef-server/ctl_chef_server_server_admin_grant_user %}}

#### Remove Members

{{% chef-server/ctl_chef_server_server_admin_remove_user %}}

#### List Membership

{{% chef-server/ctl_chef_server_server_admin_list %}}

## show-config

The `show-config` subcommand is used to view the configuration that will
be generated by the `reconfigure` subcommand. This command is most
useful in the early stages of a deployment to ensure that everything is
built properly prior to installation.

This subcommand has the following syntax:

```bash
chef-server-ctl show-config
```

## uninstall

{{% chef-server/ctl_chef_server_uninstall %}}

## upgrade

The `upgrade` subcommand is used to upgrade the Chef Infra Server.

**Syntax**

This subcommand has the following syntax:

```bash
chef-server-ctl upgrade (options)
```

**Options**

This subcommand has the following options:

`-d DIRECTORY`, `--chef11-data-dir DIRECTORY`

:   The directory in which Chef Infra Server 11 data is located. Default
    value: a temporary directory.

`-e DIRECTORY`, `--chef12-data-dir DIRECTORY`

:   The directory in which Chef Infra Server 12 data is located. Default
    value: a temporary directory.

`-f FULL_NAME`, `--full-org-name FULL_NAME`

:   The full name of the Chef Infra Server organization. The full name
    must begin with a non-white space character and must be between 1
    and 1023 characters. For example: `Chef Software, Inc.`. If this
    option is not specified, the `upgrade` command will prompt for it.

`-h`, `--help`

:   Use to show help for the `chef-server-ctl upgrade` subcommand.

`-k KEY_PATH`, `--key KEY_PATH`

:   The Chef Infra Server 11 `admin.pem` key for the API client. This is the
    key used to download Chef Infra Server 11 data. Default value:
    `/etc/chef-server/admin.pem`.

`-o ORG_NAME`, `--org-name ORG_NAME`

:   The name of the Chef Infra Server organization. The name must begin
    with a lower-case letter or digit, may only contain lower-case
    letters, digits, hyphens, and underscores, and must be between 1 and
    255 characters. For example: `chef`. If this option is not
    specified, the `upgrade` command will prompt for it.

`-s URL`, `--chef11-server-url URL`

:   The URL for the Chef Infra Server version 11. Default value:
    `https://localhost`.

`-t NUMBER`, `--upload-threads NUMBER`

:   The number of threads to use when migrating cookbooks. Default
    value: `10`.

`-u USER`, `--user`

:   Create a client as an admin client. This is required for any user to
    access Chef as an administrator.

`-x URL`, `--chef12-server-url URL`

:   The URL for the Chef Infra Server, version 12. Default value:
    `https://localhost`.

`-y`, `--yes`

:   Use to skip confirmation prompts during the upgrade process.

## Rebuild Migration Level File

The rebuild-migration-state subcommand attempts to recreate the migration-level file found at `/var/opt/opscode/upgrades/migration-level`
by inferring the current state of the database and system.

**Syntax**

This subcommand has the following syntax:

```bash
chef-server-ctl rebuild-migration-state
```

## User Management

{{% chef-server/ctl_chef_server_user %}}

### user-create

{{% chef-server/ctl_chef_server_user_create %}}

**Syntax**

{{% chef-server/ctl_chef_server_user_create_syntax %}}

**Options**

{{% chef-server/ctl_chef_server_user_create_options %}}

**Examples**

```bash
chef-server-ctl user-create john_smith John Smith john_smith@example.com p@s5w0rD!
```

```bash
chef-server-ctl user-create jane_doe Jane Doe jane_doe@example.com p@s5w0rD! -f /tmp/jane_doe.key
```

```bash
chef-server-ctl user-create waldendude Henry David Thoreau waldendude@example.com excursions
```

### user-delete

{{% chef-server/ctl_chef_server_user_delete %}}

**Syntax**

{{% chef-server/ctl_chef_server_user_delete_syntax %}}

**Examples**

```bash
chef-server-ctl user-delete john_smith
```

```bash
chef-server-ctl user-delete jane_doe
```

**Options**

This subcommand has the following options:

`-R`, `--remove-from-admin-groups`

:   Removes a user who is in one or more 'admin' groups unless that user
    is the only member of the 'admin' group(s).

### user-edit

{{% chef-server/ctl_chef_server_user_edit %}}

**Syntax**

{{% chef-server/ctl_chef_server_user_edit_syntax %}}

**Examples**

```bash
chef-server-ctl user-edit john_smith
```

```bash
chef-server-ctl user-edit jane_doe
```

### user-list

{{% chef-server/ctl_chef_server_user_list %}}

**Syntax**

{{% chef-server/ctl_chef_server_user_list_syntax %}}

**Options**

{{% chef-server/ctl_chef_server_user_list_options %}}

### user-show

{{% chef-server/ctl_chef_server_user_show %}}

**Syntax**

{{% chef-server/ctl_chef_server_user_show_syntax %}}

**Options**

{{% chef-server/ctl_chef_server_user_show_options %}}

## Service Subcommands

{{% chef-server/ctl_common_service_subcommands %}}

{{< warning >}}

The following commands are disabled when an external PostgreSQL database
is configured for the Chef Infra Server: `hup`, `int`, `kill`, `once`,
`restart`, `start`, `stop`, `tail`, and `term`.

{{< /warning >}}

### hup

{{% chef-server/ctl_chef_server_hup %}}

### int

{{% chef-server/ctl_chef_server_int %}}

### kill

{{% chef-server/ctl_chef_server_kill %}}

### once

{{% chef-server/ctl_chef_server_once %}}

### restart

{{% chef-server/ctl_chef_server_restart %}}

### service-list

{{% chef-server/ctl_chef_server_service_list %}}

### start

{{% chef-server/ctl_chef_server_start %}}

### status

{{% chef-server/ctl_chef_server_status %}}

#### Log Files

{{% chef-server/ctl_chef_server_status_logs %}}

### stop

{{% chef-server/ctl_chef_server_stop %}}

### tail

{{% chef-server/ctl_chef_server_tail %}}

### term

{{% chef-server/ctl_chef_server_term %}}
