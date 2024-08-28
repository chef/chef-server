+++
title = "Monitor"
draft = false

gh_repo = "chef-server"

aliases = ["/server_monitor.html", "/runbook/server_monitor/"]

[menu]
  [menu.server]
    title = "Monitor"
    identifier = "server/manage/Monitor"
    parent = "server/manage"
    weight = 30
+++

Monitoring the Chef Infra Server involves two types of checks:
application and system. In addition monitoring the HTTP requests that
workstations and nodes are making to the Chef Infra Server and per-disk
data storage volumes is recommended.

## Monitoring Priorities

The following sections describe the priorities for monitoring of the
Chef Infra Server. In particular, running out of disk space is the
primary cause of failure.

### Disks

Over time, and with enough data, disks will fill up or exceed the
per-disk quotas that may have been set for them and they will not be
able to write data. A disk that is not able to write data will not be
able to support certain components of the Chef Infra Server, such as
PostgreSQL, service log files, and deleted file handles.
Monitoring disk usage is the best way to ensure that disks don't fill up
or exceed their quota.

Use the following commands to monitor global disk usage on a Chef Infra
Server with a typical installation:

```bash
du -sh /var/opt/opscode
```

and:

```bash
du -sh /var/log/opscode
```

To keep the Chef Infra Server healthy, both `/var/opt/opscode` and
`/var/log/opscode` should never exceed 80% use. In situations where disk
space grows at a rapid pace, it may be preferable to shut down the Chef
Infra Server and contact Chef support.

The following components should be monitored for signs that disks may be
rapidly filling up:

-   **PostgreSQL** PostgreSQL is the data store for the Chef Infra
    Server.
-   **Log files** If `/var/log/opscode` is taking up a lot of disk
    space, ensure that the Chef Infra Server log rotation cron job is
    running without errors. These errors can be found in
    `/var/log/messages`, `/var/log/syslog` and/or the root user's local
    mail.
-   **Deleted file handles** Running processes with file handles
    associated with one (or more) deleted files will prevent the disk
    space being used by the deleted files from being reclaimed. Use the
    `sudo lsof | grep '(deleted)'` command to find all deleted file
    handles.

## Application Checks

Application-level checks should be done periodically to ensure that
there is enough disk space, enough memory, and that the front-end and
back-end services are communicating.

### Erlang

Many components of the Chef Infra Server are written using Erlang and
run on the BEAM virtual machine. One feature of Erlang and BEAM is the
ability to interact with the running service using a command shell. For
example:

```bash
cd /opt/opscode/embedded
  export PATH=$PATH:/opt/opscode/bin:/opt/opscode/embedded/bin
  bin/erl -setcookie service_name -name me@127.0.0.1 -remsh service_name@127.0.0.1
```

where `service_name` is `bifrost` or `erchef`. This command will then
open a shell that is connected to the Erchef processes:

```bash
Erlang R15B02 (erts-5.9.2) [source] [64-bit] ...
```

{{< warning >}}

Connecting to the Erlang processes should only be done when directed by
Chef support services.

{{< /warning >}}

To connect to the **oc_bifrost** service, use the following command:

```bash
erl -setcookie oc_bifrost -name me@127.0.0.1 -remsh oc_bifrost@127.0.0.1
```

To connect to the **opscode-erchef** service, use the following command:

```bash
erl -setcookie erchef -name me@127.0.0.1 -remsh erchef@127.0.0.1
```

To disconnect from the shell, use the following key sequence `CTRL-g`,
`q`, and then `ENTER`.

The output from the shell after the `CTRL-g` looks similar to:

```bash
(erchef@127.0.0.1)1>
User switch command
```

then enter `q`, and then hit `ENTER` to exit the shell.

Some commands should not be entered when interacting with a running
service while using the command shell, including:

- `q()` kills the Erlang node
- `init:stop()`
- `exit` or `exit()` does nothing

#### `eper` tools

As root on the Chef Infra Server, point to the bundled `eper` package of
debugging tools. Replace the 2nd and 5th path entries and the `X.XX.X`
value in the following path with the items that occur on the system.

```bash
export ERL_LIB=:/opt/{chef-server,opscode}/embedded/service/{erchef,opscode-erchef}/lib/eper-X.XX.X/ebin/
```

Open an Erlang command shell to begin diagnosing service issues on the
Chef Infra Server:

```bash
Eshell V5.10.4  (abort with ^G)
(erchef@127.0.0.1)1>
```

The `dtop` tool presents a view on the Erlang virtual machine that is
similar to the `linuxdagnostic` command. The period at the end of the
dtop command is required for the command to take effect.

```bash
(erchef@127.0.0.1)1> dtop:start().
```

To stop the `dtop` command, run:

```bash
(erchef@127.0.0.1)1> dtop:stop().
```

To disconnect from the shell, use the following key sequence `CTRL-g`,
`q`, and then `ENTER`.

The output from the shell after the `CTRL-g` looks similar to:

```bash
(erchef@127.0.0.1)1>
User switch command
```

then enter `q`, and then hit `ENTER` to exit the shell.

### Nginx

Use Nginx to monitor for services that may be returning 504 errors. Use
the following command on a front-end machine:

```bash
grep 'HTTP/1.1" 504' /var/log/opscode/nginx/access.log
```

and then extract the URLs and sort them by `uniq` count:

```bash
grep 'HTTP/1.1" 504' nginx-access.log | cut -d' ' -f8 | sort | uniq -c | sort
```

In a large installation, restricting these results to a subset of
results may be necessary:

```bash
tail -10000 nginx-access.log | grep 'HTTP/1.1" 504' | cut -d' ' -f8 | sort | uniq -c | sort
```

### PostgreSQL

psql is the management tool for PostgreSQL. It can be used to obtain
information about data stored in PostgreSQL. For more information about
psql, see <http://www.postgresql.org/docs/manuals/>, and then the doc
set appropriate for the version of PostgreSQL being used.

To connect to the PostgreSQL database, run the following command:

```bash
cd /opt/opscode/embedded/service/postgresql/
  export PATH=$PATH:/opt/opscode/bin:/opt/opscode/embedded/bin
  bin/psql -U opscode_chef
```

{{< warning >}}

Connecting to the PostgreSQL database should only be done when directed
by Chef support services.

{{< /warning >}}

### Redis

{{< note >}} From Chef Infra Server version 15.10.x onwards, we are using keydb instead of redis underneath. All the functions are supposed to work the same because keydb is a fork of redis. The configuration name and service name will continue to be redis_lb. {{< /note >}}

The **redis_lb** service located on the back end machine handles
requests that are made from the Nginx service that is located on all
front end machines in a Chef Infra Server cluster.

In the event of a disk full condition for the Redis data store, the
`dump.rdb` (the primary data store `.rdb` used by Redis) can become
corrupt and saved as a zero byte file.

When this occurs, after the **redis_lb** service started, it's logs
will show a statement similar to the following:

```bash
2015-03-23_16:11:31.44256 [11529] 23 Mar 16:10:09.624 # Server started, Redis version 2.8.2
2015-03-23_16:11:31.44256 [11529] 23 Mar 16:10:09.624 # WARNING overcommit_memory is set to 0! Background save may fail under low memory condition. To fix this issue add 'vm.overcommit_memory = 1' to /etc/sysctl.conf and then reboot or run the command 'sysctl vm.overcommit_memory=1' for this to take effect.
2015-03-23_16:11:31.44257 [11529] 23 Mar 16:11:31.438 # Short read or OOM loading DB. Unrecoverable error, aborting now.
```

The `dump.rdb` file will be empty:

```bash
ls -al /var/opt/opscode/redis_lb/data/
total 20
drwxr-x--- 2 opscode opscode 4096 Mar 23 15:58 .
drwxr-x--- 4 opscode opscode 4096 Dec 22 18:59 ..
-rw-r--r-- 1 opscode opscode    0 Mar 23 15:58 dump.rdb
```

This situation is caused by a bug in Redis where saves are allowed to
succeed even when the disk has been full for some time, and not just on
edge cases where the disk becomes full as Redis is writing. To fix this
issue, do the following:

1.  Stop the **redis_lb** service:

    ```bash
    chef-server-ctl stop redis_lb
    ```

2.  Remove the corrupt files:

    ```bash
    cd /var/opt/opscode/redis_lb/data
    rm -fr *rdb
    ```

3.  Start the **redis_lb** service:

    ```bash
    chef-server-ctl start redis_lb

    less /var/log/opscode/redis_lb/current
    2015-03-23_17:05:18.82516 [28676] 23 Mar 17:05:18.825 * The server is now ready to accept connections on port 16379
    ```

4.  Reconfigure the Chef Infra Server to re-populate Redis:

    ```bash
    chef-server-ctl reconfigure
    ```

5.  Verify that Redis is re-populated, as indicated by the key
    `dl_default`:

    ```bash
    /opt/opscode/embedded/bin/redis-cli -p 16379 keys \*
    1) "dl_default"
    ```

## System Checks

System-level checks should be done for the ports and services status.

### chef-backend-ctl status

The `chef-backend-ctl status` subcommand is used to check the status of
services running in the [Chef Backend server
topology]({{< relref "install_server_ha" >}}). This command will verify the status
of the following services on the node it is run on:

-   `leaderl`
-   `postgresql`
-   `etcd`
-   `epmd`
-   `elasticsearch`

It will also check on the status of other nodes in the cluster, from the
current node's perspective. For example:

```bash
chef-backend-ctl status
Service Local Status Time in State Distributed Node Status
leaderl running (pid 1191) 53d 15h 11m 12s leader: 1; waiting: 0; follower: 2;    total: 3
epmd running (pid 1195) 53d 15h 11m 12s status: local-only
etcd running (pid 1189) 53d 15h 11m 12s health: green; healthy nodes: 3/3
postgresql running (pid 40686) 0d 12h 36m 23s leader: 1; offline: 0; syncing: 0;    synced: 2
elasticsearch running (pid 47423) 0d 12h 18m 6s state: green; nodes online: 3/3

System Local Status Distributed Node Status
disks /var/log/chef-backend: OK; /var/opt/chef-backend: OK health: green; healthy    nodes: 3/3
```

More information about each service can be found in the individual
service logs in `/var/opt/chef-backend/`.

### opscode-authz

The authz API provides a high-level view of the health of the
**opscode-authz** service with a simple endpoint: `_ping`. This endpoint
can be accessed using cURL and GNU Wget. For example:

```bash
curl http://localhost:9463/_ping
```

This command typically prints a lot of information. Use Python to use
pretty-print output:

```bash
curl http://localhost:9463/_ping | python -mjson.tool
```

### opscode-erchef

The status API provides a high-level view of the health of the system
with a simple endpoint: `_status`. This endpoint can be accessed using
cURL and GNU Wget. For example:

```bash
curl http://localhost:8000/_status
```

which will return something similar to:

```bash
{
  "status":"pong",
  "upstreams":{"upstream_service":"pong","upstream_service":"fail",...},
}
```

For each of the upstream services, `pong` or `fail` is returned. The
possible upstream names are:

- `chef_sql` (for the **postgresql** service)
- `oc_chef_authz` (for the **opscode-authz** service)

If any of the status values return `fail`, this typically means the Chef
Infra Server is unavailable for that service.

## Nodes, Workstations

If a client makes an HTTP request to the server that returns a
non-specific error message, this is typically an issue with the
**opscode-chef** or **opscode-erchef** services. View the full error
message for these services in their respective log files. The error is
most often a stacktrace from the application error. In some cases, the
error message will clearly indicate a problem with another service,
which can then be investigated further. For non-obvious errors, please
contact Chef support services.
