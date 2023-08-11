+++
title = "Server Services"
draft = false

gh_repo = "chef-server"

aliases = ["/server_services.html", "/runbook/server_services/"]

[menu]
  [menu.server]
    title = "Services"
    identifier = "server/overview/Services"
    parent = "server/overview"
    weight = 60
+++

The Chef Infra Server has a built in process supervisor, which ensures
that all of the required services are in the appropriate state at any
given time. The supervisor starts two processes per service.

## Service Subcommands

{{< readfile file="content/server/reusable/md/ctl_common_service_subcommands.md" >}}

### hup

{{< readfile file="content/server/reusable/md/ctl_chef_server_hup.md" >}}

### int

{{< readfile file="content/server/reusable/md/ctl_chef_server_int.md" >}}

### kill

{{< readfile file="content/server/reusable/md/ctl_chef_server_kill.md" >}}

### once

{{< readfile file="content/server/reusable/md/ctl_chef_server_once.md" >}}

### restart

{{< readfile file="content/server/reusable/md/ctl_chef_server_restart.md" >}}

### service-list

{{< readfile file="content/server/reusable/md/ctl_chef_server_service_list.md" >}}

### start

{{< readfile file="content/server/reusable/md/ctl_chef_server_start.md" >}}

### status

{{< readfile file="content/server/reusable/md/ctl_chef_server_status.md" >}}

#### Log Files

{{< readfile file="content/server/reusable/md/ctl_chef_server_status_logs.md" >}}

### stop

{{< readfile file="content/server/reusable/md/ctl_chef_server_stop.md" >}}

### tail

{{< readfile file="content/server/reusable/md/ctl_chef_server_tail.md" >}}

### term

{{< readfile file="content/server/reusable/md/ctl_chef_server_term.md" >}}

## List of Services

The following services are part of the Chef Infra Server:

- bifrost
- bookshelf
- elasticsearch
- nginx
- opscode-erchef
- postgresql
- redis-lb

### bifrost

{{< readfile file="content/server/reusable/md/server_services_bifrost.md" >}}

#### status

To view the status for the service:

```bash
chef-server-ctl status bifrost
```

to return something like:

```bash
run: bifrost: (pid 1234) 123456s; run: log: (pid 5678) 789012s
```

#### start

To start the service:

```bash
chef-server-ctl start bifrost
```

#### stop

To stop the service:

```bash
chef-server-ctl stop bifrost
```

#### restart

To restart the service:

```bash
chef-server-ctl restart bifrost
```

to return something like:

```bash
ok: run: bifrost: (pid 1234) 1234s
```

#### kill

To kill the service (send a `SIGKILL` command):

```bash
chef-server-ctl kill bifrost
```

#### run once

To run the service, but not restart it (if the service fails):

```bash
chef-server-ctl once bifrost
```

#### tail

To follow the logs for the service:

```bash
chef-server-ctl tail bifrost
```

### bookshelf

{{< readfile file="content/server/reusable/md/server_services_bookshelf.md" >}}

#### status

To view the status for the service:

```bash
chef-server-ctl status bookshelf
```

to return something like:

```bash
run: bookshelf: (pid 1234) 123456s; run: log: (pid 5678) 789012s
```

#### start

To start the service:

```bash
chef-server-ctl start bookshelf
```

#### stop

To stop the service:

```bash
chef-server-ctl stop bookshelf
```

#### restart

To restart the service:

```bash
chef-server-ctl restart bookshelf
```

to return something like:

```bash
ok: run: bookshelf: (pid 1234) 1234s
```

#### kill

To kill the service (send a `SIGKILL` command):

```bash
chef-server-ctl kill bookshelf
```

#### run once

To run the service, but not restart it (if the service fails):

```bash
chef-server-ctl once bookshelf
```

#### tail

To follow the logs for the service:

```bash
chef-server-ctl tail bookshelf
```

### Elasticsearch

#### status

To view the status for the service:

```bash
chef-server-ctl status elasticsearch
```

to return something like:

```bash
elasticsearch: (pid 12345) 1s; run: log: (pid 5678) 123456s
```

#### start

To start the service:

```bash
chef-server-ctl start elasticsearch
```

to return something like:

```bash
ok: run: elasticsearch: (pid 5678) 0s
```

#### stop

To stop the service:

```bash
chef-server-ctl stop elasticsearch
```

to return something like:

```bash
ok: down: elasticsearch: 123456s, normally up
```

#### restart

To restart the service:

```bash
chef-server-ctl restart elasticsearch
```

to return something like:

```bash
ok: run: elasticsearch: (pid 56789) 1s
```

#### kill

To kill the service (send a `SIGKILL` command):

```bash
chef-server-ctl kill elasticsearch
```

#### run once

```bash
chef-server-ctl once elasticsearch
```

#### tail

To follow the logs for the service:

```bash
chef-server-ctl tail elasticsearch
```

### nginx

{{< readfile file="content/server/reusable/md/server_services_nginx.md" >}}

#### status

To view the status for the service:

```bash
chef-server-ctl status nginx
```

to return something like:

```bash
run: nginx: (pid 1234) 123456s; run: log: (pid 5678) 789012s
```

#### start

To start the service:

```bash
chef-server-ctl start nginx
```

#### stop

To stop the service:

```bash
chef-server-ctl stop nginx
```

#### restart

To restart the service:

```bash
chef-server-ctl restart nginx
```

to return something like:

```bash
ok: run: nginx: (pid 1234) 1234s
```

#### kill

To kill the service (send a `SIGKILL` command):

```bash
chef-server-ctl kill nginx
```

#### run once

To run the service, but not restart it (if the service fails):

```bash
chef-server-ctl once nginx
```

#### tail

{{< readfile file="content/server/reusable/md/server_services_nginx_tail.md" >}}

### opscode-erchef

{{< readfile file="content/server/reusable/md/server_services_erchef.md" >}}

#### status

To view the status for the service:

```bash
chef-server-ctl status opscode-erchef
```

to return something like:

```bash
run: opscode-erchefs: (pid 1234) 123456s; run: log: (pid 5678) 789012s
```

#### start

To start the service:

```bash
chef-server-ctl start opscode-erchef
```

#### stop

To stop the service:

```bash
chef-server-ctl stop opscode-erchef
```

#### restart

To restart the service:

```bash
chef-server-ctl restart opscode-erchef
```

to return something like:

```bash
ok: run: opscode-erchef: (pid 1234) 1234s
```

#### kill

To kill the service (send a `SIGKILL` command):

```bash
chef-server-ctl kill opscode-erchef
```

#### run once

To run the service, but not restart it (if the service fails):

```bash
chef-server-ctl once opscode-erchef
```

#### tail

To follow the logs for the service:

```bash
chef-server-ctl tail opscode-erchef
```

### postgresql

{{< readfile file="content/server/reusable/md/server_services_postgresql.md" >}}

#### status

To view the status for the service:

```bash
chef-server-ctl status postgresql
```

to return something like:

```bash
run: postgresql: (pid 1234) 123456s; run: log: (pid 5678) 789012s
```

#### start

To start the service:

```bash
chef-server-ctl start postgresql
```

#### stop

To stop the service:

```bash
chef-server-ctl stop postgresql
```

#### restart

To restart the service:

```bash
chef-server-ctl restart postgresql
```

to return something like:

```bash
ok: run: postgresql: (pid 1234) 1234s
```

#### kill

To kill the service (send a `SIGKILL` command):

```bash
chef-server-ctl kill postgresql
```

#### run once

To run the service, but not restart it (if the service fails):

```bash
chef-server-ctl once postgresqls
```

#### tail

To follow the logs for the service:

```bash
chef-server-ctl tail postgresql
```

### redis

{{< readfile file="content/server/reusable/md/server_services_redis.md" >}}

#### status

To view the status for the service:

```bash
chef-server-ctl status redis
```

to return something like:

```bash
run: redis: (pid 1234) 123456s; run: log: (pid 5678) 789012s
```

#### start

To start the service:

```bash
chef-server-ctl start redis
```

#### stop

To stop the service:

```bash
chef-server-ctl stop redis
```

#### restart

To restart the service:

```bash
chef-server-ctl restart redis
```

to return something like:

```bash
ok: run: redis: (pid 1234) 1234s
```

#### kill

To kill the service (send a `SIGKILL` command):

```bash
chef-server-ctl kill name_of_service
```

#### run once

To run the service, but not restart it (if the service fails):

```bash
chef-server-ctl once redis
```

#### tail

To follow the logs for the service:

```bash
chef-server-ctl tail name_of_service
```
