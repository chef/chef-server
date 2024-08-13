Key-value store used in conjunction with Nginx to route requests and
populate request data used by the Chef Infra Server.

{{< note >}} From Chef Infra Server version 15.10.12 onwards, we are using keydb instead of redis underneath. All the functions are supposed to work the same because keydb is a fork of redis. The configuration name and service name will continue to be redis_lb.{{< /note >}}