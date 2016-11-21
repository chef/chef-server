# Openresty Production Deployment Plan

This is the plan to deploy the `openresty` load balancer to production as a replacement for `nginx`.

## Overview

Openresty is a "distribution" of Nginx, packaging it up with plugins that enable Lua and Redis access from within the config, among other things.

First, we'll "bake-test" an openresty load balancer to look for exceptional resource utilization and errors. After that, we'll proceed to replace nginx with openresty in-place on the existing load balancers.

## Preparation

### 1. Code Merge

The OpenResty / Lua work has been combined into one pull request:

* https://github.com/opscode/opscode-platform-cookbooks/pull/112

This will need to be merged before deploy. In order to simplify a potential rollback, prepend the first line of the merge commit with the following:

```
openresty-merge-commit
```

### 2. CCR Suspend

We'll want to permanently suspend chef-client runs on the the existing load balancers for the duration of the Openresty deploy. Once we upload the new cookbooks / roles / databags to the Chef Server (opsmaster) we'll be unalbe to run chef-client unless we roll back the cookbooks / roles / databags.

```bash
knife exec -E 'search(:node, "role:opscode-lb* OR role:copsite-lb").each{|n| n.tags << "suicide"; n.save}'
```

Adding the `suicide` tag will ensure that if / when the node resumes daemonized chef-client runs (started by cron) that they will exit early and have no effect.

### 3. Knife Upload

```bash
knife upload cookbooks/opscode-lb \
             cookbooks/openresty \
             data/xdarklaunch/rs-prod.json \
```

### 4. Resource Planning - Load Balancer Distribution

Below is a list of the virt hosts that have a load balancer depoyed to them.

**NOTE:** rm-478882 is the target node for the test-bake deploy.

* rm-478864 - lb-rsprod-rv-def10b68
* rm-423003 - lb-rsprod-rv-cc67526b
* rm-478880 - lb-rsprod-rv-8ef1f3d1 (chef restarter)
* rm-478884 - lbcorp-rsprod-rv-085d4ac0
* rm-422842 - lb-rsprod-rv-e0ce9d29
* rm-478881 - lbcorp-rsprod-rv-b5205de2
* rm-478882 - *test-bake-target*
* rm-478885 - lb-rsprod-rv-615143f3

## Deploy - External Test Bake

The Openresty load balancer has been tested in preprod for correctness, but we'd like to get an idea in production of how it (combined with Lua) performs with "real" traffic. The plan is to insert one load openresty load balancer into the roatation alongside the existing nginx load balancers and watch for errors and exceptional resource utilization.

We will let the new server bake for one day (overnight) before deploying openresty to the rest(y) of the fleet.

**Note:** `ocvmb` does not currently install Chef from omnibus: [link](https://tickets.corp.chef.io/browse/OC-7369)

The plan:

1. Create the VM

   On a server without an existing load balancer, use `ocvmb` to create a new external load balancer. ([`ocvmb` docs on wiki](http://wiki.corp.chef.io/display/CORP/Rackspace+Production#RackspaceProduction-CreatingVMsusing%7B%7Bocvmb%7D%7D))

   SSH to the host (use the one below or pick a different empty one):

   ```bash
   ssh rm-366625.opscode.us
   ```

   Run `ocvmb` to generate the config:

   ```bash
   ocvmb
   ```

   Run `ocvmb` to create and bootstrap the load balancer:

   ```bash
   ocvmb 'role[rs-prod], role[opscode-lb]'
   ```

1. Remove the `ubuntu` User

   The default user on ubuntu servers is named `ubuntu` and has default password of `ubuntu`. Since these load balancers will be connected directly to the Internet, we'll need to login with an alternate user (all created by the first chef-client run) and run chef-client, which will delete the default user.

   SSH to the new server:

   ```bash
   sudo chef-client
   ```

1. Add NAT Rule to ASA Firewall

   Once the ubuntu user has been removed from the load balancer, we can safely connect it to the Internet by adding a NAT rule to the Cisco ASA Firewalls in Rackspace.

   *Nathan, Pauly, or Ian can do this.*

1. Pedant Test the New Load Balancer

   Once the load balancer has been connected to the Internet, we will want to run pedant tests against it to verify that the functionality is correct.

   **NOTE:** Replace `NEW_LB_FQDN` below with the FQDN of the new load balancer.

   ```bash
   cd ~/oc/environments/rs-prod/pedant
   ./bin/ohc-pedant -e rs-prod -s NEW_LB_FQDN -- --smoke
   ```

1. Add the load balancer into Dynect DNS:

   Once the correctness of the load balancer has been validated, we can add it into the DNS rotation.

   *Nathan, Pauly, or Ian can do this.*

1. Monitor Errors / Resource Utilization

   While the load balancer is serving prodiction traffic, monitor that there are not an abnormal number of errors coming from this node and that it is not using exceptionally more resources than the other load balancers.

## Bake Time - Problem Resolution

Since we intend to have a test run of openresty that goes overnight, we don't want to be left in a position where we are unable to deploy new code to the existing load balancers. Here are the steps to roll back the state of the chef repo in order to deploy new code:

```bash
# delete the uploaded opscode-lb cookbook
knife cookbook delete opscode-lb 1.1.0

# revert the merge commit (the merge will have special text to help facilitate matching)
git revert --no-edit `git log -n 1 --grep="openresty-merge-commit" --format=%H`

# remove the suicide flag from the nginx nodes
knife exec -E 'search(:node, "role:opscode-lb* OR role:corpsite-lb").each{|n| n.tags.delete "suicide"; n.save}'

# add the suicide flag to the openresty node
knife exec -E 'search(:node, "openresty:*").each{|n| n.tags << "suicide"; n.save}'
```

You will now be able to safely deploy new code to the nginx load balancers without deploying openresty at the same time.

## Deploy - External

Once we're satisfied with performance and correctness of the initial Openresty load balancer, we can replace the Nginx on the remaining nodes with Openresty.

To find the Nginx nodes:

```
knife search node 'role:opscode-lb OR role:corpsite-lb`
```

Do the following for each of the Nginx nodes (start with opscode-lb then move to corpsite-lb):


**NOTE:** The node name will bereferred to as `$NODE_NAME` below.

```bash
# node names look like "rv-123123"
export NODE_NAME=insert_node_name_here
```

1. Take the Node Out of Dynect DNS Rotation

   To stop traffic to the load balancer we will first need to take it out of the Dynect DNS rotation.

   *Nathan, Pauly, or Ian can do this.*

1. Nginx - Stop the `nginx` Service

   Once the traffic has stopped on the load balancer we can safely stop the Nginx service.

   ```bash
   sudo /etc/init.d/nginx stop
   ```

1. Remove the `:nginx` Node Attributes

   The `openresty` cookbook depends on node attributes with the key `:openresty`. The old `nginx` cookbook used `:nginx` as the key. The `openresty` cookbook also ships an ohai plugin that writes to the `:nginx` node attribute. In order to prevent potential confusion, we should delete the previous `:nginx` node attributes before we deploy OpenResty.

   ```bash
   knife node edit $NODE_NAME
   ```

   Remove the `"nginx"` key and all of it's nested attributes from the JSON.

1. Deploy Openresty

   ```bash
   sudo chef-client
   ```

1. Pedant Test the New Load Balancer

   Once Openresty has been build and configured we will want to run pedant tests against it to verify that the functionality is correct.

   **NOTE:** Replace `NEW_LB_FQDN` below with the FQDN of the new load balancer.

   ```bash
   cd ~/oc/environments/rs-prod/pedant
   ./bin/ohc-pedant -e rs-prod -s NEW_LB_FQDN -- --smoke
   ```

1. Add the Node Back Into Dynect DNS

   *Nathan, Pauly, or Ian can do this.*

## Deploy - Internal #1 (Inactive)

The internal load balancers are an active-inactive pair. We'll first deploy OpenResty onto the inactive load balancer.

To determine the active and inactive internal load balancers:

```bash
ssh api.opscode.us 'sudo crm status'
```

The output should look like:

```
============
Last updated: Mon Apr 15 23:57:53 2013
Stack: Heartbeat
Current DC: lbint-rsprod-rm-319425 (5c14d79c-74ac-461d-9f9f-d5fd0acc5c7e) - partition with quorum
Version: 1.0.8-042548a451fce8400660f6031f4da6f0223dd5dd
2 Nodes configured, unknown expected votes
5 Resources configured.
============

Online: [ lbint-rsprod-rm-319425 lbint-rsprod-rm-319424 ]

 res_IPaddr_1   (ocf::heartbeat:IPaddr):        Started lbint-rsprod-rm-319425
 res_nginx_1    (lsb:nginx):    Started lbint-rsprod-rm-319425
 st-drac-319424 (stonith:external/ipmi):        Started lbint-rsprod-rm-319425
 st-drac-319425 (stonith:external/ipmi):        Started lbint-rsprod-rm-319424
 res_email_1    (ocf::heartbeat:MailTo):        Started lbint-rsprod-rm-319425
```

In the above case, `rm-319425` is the active node because it has the `IPaddr` resource, which is the internal load balancer's VIP (virtual IP). We'll start on the inactive node: `rm-319424`.

1. Nginx - Stop the `nginx` Service

   Once the traffic has stopped on the load balancer we can safely stop the Nginx service.

   ```bash
   sudo /etc/init.d/nginx stop
   ```

1. Remove the `:nginx` Node Attributes

   The `openresty` cookbook depends on node attributes with the key `:openresty`. The old `nginx` cookbook used `:nginx` as the key. The `openresty` cookbook also ships an ohai plugin that writes to the `:nginx` node attribute. In order to prevent potential confusion, we should delete the previous `:nginx` node attributes before we deploy OpenResty.

   ```bash
   knife node edit $NODE_NAME
   ```

   Remove the `"nginx"` key and all of it's nested attributes from the JSON.

1. Deploy Openresty

   ```bash
   sudo chef-client
   ```

1. Pedant Test the New Load Balancer

   Once Openresty has been build and configured we will want to run pedant tests against it to verify that the functionality is correct.

   **NOTE:** Replace `NEW_LB_FQDN` below with the FQDN of the new load balancer.

   ```bash
   cd ~/oc/environments/rs-prod/pedant
   ./bin/ohc-pedant -e rs-prod -s NEW_LB_FQDN -- --smoke
   ```

1. Failover

   Once OpenResty has been deployed and tested on the inactive internal load balancer, we need to fail over the resources from the active load balancer.

   On the active load balancer:

   ```
   sudo /etc/init.d/heartbeat stop
   ```

## Deploy - Internal #2 (Previous Active)

First, repeat steps 1-4 from above.

1. Start Hearbeat

   Once the functionality of the load balancer has been verified with `pedant`, we need to add it back into the HA cluster.

   ```
   sudo /etc/init.d/heartbeat start
   ```
