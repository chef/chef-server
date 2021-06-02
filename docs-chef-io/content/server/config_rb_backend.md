+++
title = "chef-backend.rb Settings"
draft = false

gh_repo = "chef-server"

aliases = ["/config_rb_backend.html", "/config_rb_backend/"]

[menu]
  [menu.server]
    title = "chef-backend.rb Settings"
    identifier = "server/configure/chef-backend.rb Settings"
    parent = "server/configure"
    weight = 30
+++

{{% EOL_backend %}}

## chef-backend.rb Options

{{% config_rb_backend_summary %}}

### Common 'Runit' Flags for Any Backend Service

See <https://github.com/chef-cookbooks/runit> for details. Many of the
flags are repeated across the various backend services, but they are only
documented once at the top here. The same defaults are used unless
specified below.

`postgresql.enable`
: Sets up and runs the postgresql service.
: Default value: `true`.

`postgresql.environment`
: A hash of environment variables with their
  values as content used in the service's env directory.

`postgresql.log_directory`
: The directory where the `svlogd` log service will run.
: Default value: `'/var/log/chef-backend/postgresql/<version>'`.

`postgresql.log_rotation.file_maxbytes`
: The maximum size a log file can grow to before it is automatically rotated.
: Default value: `104857600` (100MB).

`postgresql.log_rotation.num_to_keep`
: The maximum number of log files that will be retained after rotation.
: Default value: `10`.

`etcd.enable`
: Sets up and runs the etcd service.
: Default value: `true`.

`etcd.log_directory`
: The directory where the `svlogd` log service will run.
: Default value: `'/var/log/chef-backend/etcd'`.

`etcd.log_rotation.file_maxbytes`
: The maximum size a log file can grow to before it is automatically rotated.
: Default value: `104857600` (100MB).

`etcd.log_rotation.num_to_keep`
: The maximum number of log files that will be retained after rotation.
: Default value: `10`.

`elasticsearch.enable`
: Sets up and runs the elasticsearch service.
: Default value: `true`.

`elasticsearch.log_directory`
: The directory where the `svlogd` log service will run.
  Also affects `path.logs` in the Elasticsearch configuration YAML.
: Default value: `'/var/log/chef-backend/elasticsearch'`.

`elasticsearch.log_rotation.file_maxbytes`
: The maximum size a log file can grow to before it is automatically rotated.
: Default value: `104857600` (100MB).

`elasticsearch.log_rotation.num_to_keep`
: The maximum number of log files that will be retained after rotation.
: Default value: `10`.

`leaderl.enable`
: Sets up and runs the leaderl service.
: Default value: `true`.

`leaderl.log_directory`
: The directory where the `svlogd` log service will run.
: Default value: `'/var/log/chef-backend/leaderl'`.

`leaderl.start_down`
: The default state of the runit service to `down` by creating `<sv_dir>/down` file.
: Default value: `true`.

`leaderl.log_rotation.file_maxbytes`
: The maximum size a log file can grow to before it is automatically rotated.
: Default value: `104857600` (100MB).

`leaderl.log_rotation.num_to_keep`
: The maximum number of log files that will be retained after rotation.
: Default value: `10`.

### PostgreSQL Settings

`postgresql.db_superuser`
: Super user account to create. Password is in `chef-backend-secrets.json`.
: Default value: `'chef_pgsql'`.

`postgresql.md5_auth_cidr_addresses`
: A list of authorized addresses
  from which other backend nodes can connect to perform streaming
  replication. `samehost` and `samenet` are special symbols to allow
  connections from the this node's IP address and its subnet. You may
  also use `all` to match any IP address. You may specify a hostname
  or IP address in CIDR format (`172.20.143.89/32` for a single host,
  or `172.20.143.0/24` for a small network. See
  <https://www.postgresql.org/docs/9.5/static/auth-pg-hba-conf.html>
  for alternative formats.
: Default value: `["samehost", "samenet"]`.

`postgresql.replication_user`
: Username used by postgres streaming replicator when accessing this node.
: Default value: `'replicator'`.

`postgresql.username`
: System username that the postgres process will run as.
: Default value: `'chef_pgsql'`.

### PostgreSQL Settings Given to `postgresql.conf`

See [PostgreSQL's documentation](https://www.postgresql.org/docs/9.5/static/runtime-config.html)
for details. Some defaults are provided:

`postgresql.archive_command`
: Default value: `''`.

`postgresql.archive_mode`
: Default value: `'off'`.

`postgresql.archive_timeout`
: Default value: `0`.

`postgresql.checkpoint_completion_target`
: Default value: `0.5`.

`postgresql.checkpoint_timeout`
: Default value: `'5min'`.

`postgresql.checkpoint_warning`
: Default value: `'30s'`.

`postgresql.effective_cache_size`
: Automatically calculated based on available memory.

`postgresql.hot_standby`
: Default value: `'on'`.

`postgresql.keepalives_count`
: Sets `tcp_keepalives_count`.
: Default value: `2`.

`postgresql.keepalives_idle`
: Sets `tcp_keepalives_idle`.
: Default value: `60`.

`postgresql.keepalives_interval`
: Sets `tcp_keepalives_interval`.
: Default value: `15`.

`postgresql.log_checkpoints`
: Default value: `true`.

`postgresql.log_min_duration_statement`
: Default value: `-1`.

`postgresql.max_connections`
: Default value: `350`.

`postgresql.max_replication_slots`
: Default value: `12`.

`postgresql.max_wal_senders`
: Default value: `12`.

`postgresql.max_wal_size`
: Default value: `64`.

`postgresql.min_wal_size`
: Default value: `5`.

`postgresql.port`
: Default value: `5432`.

`postgresql.shared_buffers`
: Automatically calculated based on available memory.

`postgresql.wal_keep_segments`
: Default value: `32`.

`postgresql.wal_level`
: Default value: `'hot_standby'`.

`postgresql.wal_log_hints`
: Default value: `on`.

`postgresql.work_mem`
: Default value: `'8MB'`.

### etcd Settings

`etcd.client_port`
: Port to use for ETCD_LISTEN_CLIENT_URLS and ETCD_ADVERTISE_CLIENT_URLS.
: Default value: `2379`.

`etcd.peer_port`
: Port to use for ETCD_LISTEN_PEER_URLS and ETCD_ADVERTISE_PEER_URLS.
: Default value: `2380`.

The following settings relate to etcd's consensus protocol. Chef Backend
builds its own leader election on top of etcd's consensus protocol.
Updating these settings may be advisable if you are seeing frequent
failover events as a result of spurious etcd connection timeouts. The
current defaults assume a high-latency environment, such as those you might
find if deploying Chef Backend to various cloud providers.

`etcd.heartbeat_interval`
: ETCD_HEARTBEAT_INTERVAL in
  milliseconds. This is the frequency at which the leader will send
  heartbeats to followers. etcd's documentation recommends that this
  is set roughly to the round-trip times between members.
: Default value: `500`, (`100` by default before Chef Backend 1.2).

`etcd.election_timeout`
: ETCD_ELECTION_TIMEOUT in milliseconds. This controls how long an etcd node will
  wait for heartbeat before triggering an election. Per etcd's documentation,
  this should be 5 to 10 times larger than the
  `etcd.heartbeat_interval`. Increasing `etcd.election_timeout`
  increases the time it will take for `etcd` to detect a failure.
: Default value: `5000`, (`1000` by default before Chef Backend 1.2).

`etcd.snapshot_count`
: ETCD_SNAPSHOT_COUNT which is the number of committed transactions to trigger a snapshot to disk.
: Default value: `5000`.

`etcd.ionice.class`
: etcd must be able to write to disk with minimal
  latency. If your cluster does not meet the
  [disk requirements]({{< relref "install_server_ha/#hardware-requirements" >}}), e.g. you
  are running Chef Backend on virtual machines with shared disks, this
  settings should be changed to '1' (real-time scheduling) to mitigate
  unnecessary failovers under high latency conditions.
: Default value: `2`.
: New in Chef Backend 2.2.

`etcd.ionice.level`
: This may be changed for further tuning to specific environments if `etcd.ionice.class`
  is changed, but in almost all cases it should not be modified.
: Default value: `0`.
: New in Chef Backend 2.2.

{{< note >}}

Even though the defaults assume a high-latency environment, cloud
deployments should be restricted to the same datacenter, or in AWS, in
the same region. This means that geographically-dispersed cluster
deployments are not supported. Multiple Availability Zones *are*
supported as long as they are in the same region.

{{< /note >}}

See [etcd's documentation on tunables](https://etcd.io/docs/latest/tuning/)
for more information.

### Elasticsearch JVM Settings

`elasticsearch.heap_size`
: Automatically computed by Elasticsearch based on available memory.
  Specify in MB if you wish to override.

`elasticsearch.java_opts`
: Flags to directly pass to the JVM when launching Elasticsearch.
  If you override a heap flag here, the setting here takes precedence.

`elasticsearch.new_size`
: Java heap's new generation size.

### Elasticsearch Configuration

See [Elasticsearch's configuration documentation](https://www.elastic.co/guide/en/elasticsearch/reference/current/settings.html) for additional information.

`elasticsearch.plugins_directory`
: Sets the `path.plugins` value.
: Default value: `'/var/opt/chef-backend/elasticsearch/plugins'`.

`elasticsearch.port`
: Sets the `http.port` value.
: Default value: `9200`.

`elasticsearch.scripts_directory`
: Sets the `path.scripts` value.
: Default value: `'/var/opt/chef-backend/elasticsearch/scripts'`.

### Chef HA Backend Leader Management Service Settings


`leaderl.db_timeout`
: Socket timeout when connecting to PostgreSQL in
  milliseconds.
: Default value: `2000`.

`leaderl.http_acceptors`
: HTTP threads that responds to monitoring
  and leadership status requests from HAProxy.
: Default value: `10`.

`leaderl.http_address`
: The address that leaderl listens on. This
  address should not be `127.0.0.1`. It should be reachable from any
  front-end node.
: Default value: `'0.0.0.0'`.

`leaderl.http_port`
: Default value: `7331`.

`leaderl.leader_ttl_seconds`
: The number of seconds it takes the
  leader key to expire. Increasing this value will increase the amount
  of time the cluster will take to recognize a failed leader. Lowering
  this value may lead to frequent leadership changes and thrashing.
: Default value: `30`. (`10` by default before Chef Backend 1.2).

`leaderl.required_active_followers`
: The number of followers that
  must be syncing via a PostgreSQL replication slot before a new
  leader will return 200 to /leader HTTP requests. If an existing
  leader fails to maintain this quorum of followers, the /leader
  endpoint will return 503 but active connections will still be able
  to complete their writes to the database.
: Default value: `0`.

`leaderl.runsv_group`
: The group that sensitive password files will
    belong to. This is used internally for test purposes and should
    never be modified otherwise.
: Default value: `'chef_pgsql'`.

`leaderl.status_internal_update_interval_seconds`
: How often we check for a change in the leader service's status in seconds.
: Default value: `5`.

`leaderl.status_post_update_interval_seconds`
: How often etcd is updated with the leader service's current status in seconds.
: Default value: `10`.

`leaderl.username`
: System username that the leaderl process will run as. Usually the same as `postgresql.username`.
: Default value: `'chef_pgsql'`.

`leaderl.log_rotation.max_messages_per_second`
: Rate limit for the number of messages that the Erlang error_logger will output.
: Default value: `1000`.

`leaderl.etcd_pool.ibrowse_options`
: Internal options to affect how requests to etcd are made (see
  <https://github.com/cmullaparthi/ibrowse/blob/master/doc/ibrowse.html>).

`leaderl.epmd_monitor.check_interval`
: How often to check that leaderl is registered with the Erlang Port Mapping Daemon (epmd) in milliseconds.
: Default value: `60000`.

### Chef HA Backend Leader Health Status Settings

`leaderl.health_check.interval_seconds`
: How frequently to poll the service for health status in seconds. We recommend setting this to
  at least 5 times the value of `leaderl.leader_ttl_seconds`.
: Default value: `5`, (`2` by default before version Chef Backend 1.2).

`leaderl.health_check.max_bytes_behind_leader`
: Limit on maximum different between elected leader and current node in bytes.
: Default value: `52428800` (50MB).

`leaderl.health_check.max_elasticsearch_failures`
: Number of Elasticsearch API failures allowed before health check fails.
: Default value: `5`.

`leaderl.health_check.max_etcd_failures`
: Number of etcd failures allowed before health check fails.
: Default value: `5`.

`leaderl.health_check.max_pgsql_failures`
: Number of PostgreSQL connection failures allowed before health check fails.
: Default value: `5`.

`leaderl.health_check.fatal_system_checks`
: Whether or not system
  check failures (such as disk space failures) will result in the node
  being marked ineligible for leadership.
: Default value: `false`.
: New in Chef Backend 1.4.

`leaderl.health_check.disk_paths`
: An array containing the paths to check for sufficient disk space.
: Default value: `[/var/log/chef-backend, /var/opt/chef-backend]`.
: New in Chef Backend 1.4.

`leaderl.health_check.disk_min_space_mb`
: The minimum amount of disk space (in megabytes) required for a disk health check to pass.
: Default value: `250`.
: New in Chef Backend 1.4.

### Chef HA Backend Leader Connection Pool Settings

See <https://github.com/seth/pooler/blob/master/README.org> for details.
These are internal settings that affect the responsiveness, uptime and
reliability of the backend cluster. They should not be modified unless
you are advised to do so by Support.

`leaderl.etcd_pool.cull_interval_seconds`
: Default value: `60`.

`leaderl.etcd_pool.http_timeout_ms`
: Default value: `5000`.

`leaderl.etcd_pool.init_count`
: Default value: `10`.

`leaderl.etcd_pool.max_age_seconds`
: Default value: `60`.

`leaderl.etcd_pool.max_connection_duration_seconds`
: Default value: `300`.

`leaderl.etcd_pool.max_count`
: Default value: `10`.

### SSL Settings

If `certificate` and `certificate_key` are nil, the SSL Certificate will
be auto-generated using the other parameters provided. Otherwise, they
are on-disk locations to user-provided certificate.

`ssl.certificate`
: Provide this path if you have a pre-generated SSL cert.

`ssl.certificate_key`
: Provide this path if you have a pre-generated SSL cert.

`ssl.ciphers`
: Ordered list of allowed SSL ciphers. This will be
  updated based on security considerations and the version of OpenSSL
  being shipped.

`ssl.company_name`
: The name of your organization.

`ssl.country_name`
: The two-character country code.

`ssl.data_dir`
: Where certificates will be stored.
: Default value: `'/var/opt/chef-backend/ssl/'`.

`ssl.duration`
: The duration of the certificate in days.
: Default value: `3650` (10 years).

`ssl.key_length`
: Default value: `2048`.

`ssl.organizational_unit_name`
: The name of the division in your organization.

## chef-backend-ctl

The Chef Infra Server backend HA cluster includes a command-line utility
named chef-backend-ctl. This command-line tool is used to manage the
Chef Infra Server backend HA cluster, start and stop individual
services, and tail Chef Infra Server log files. For more information,
see the [chef-backend-ctl documentation]({{< relref "ctl_chef_backend" >}}).
