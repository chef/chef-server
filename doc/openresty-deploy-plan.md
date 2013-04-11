# Openresty Production Deployment Plan

This is the plan to deploy the `openresty` load balancer to production as a replacement for `nginx`.

## Overview

Openresty is a "distribution" of Nginx, packaging it up with plugins that enable Lua and Redis access from within the config, among other things.

Since we'll be re-building and swapping out the existing nginx binary, it will be much more predictable to build the new load balancer nodes from scratch and move them into rotation (DNS load-balanced) one at a time.

## Preparation

### 1. Code Merge

There are two pull requests that compose the work to move to Openresty:

* https://github.com/opscode/opscode-platform-cookbooks/pull/112
* https://github.com/opscode/opscode-platform-cookbooks/pull/114

Both of these will need to be merged before deploy.

### 2. CCR Suspend

We'll want to permanently suspend chef-client runs on the the existing load balancers for the duration of the Openresty deploy. Once we upload the new cookbooks / roles / databags to the Chef Server (opsmaster) we'll be unalbe to run chef-client unless we roll back the cookbooks / roles / databags.

**Note:** `suicide-ctl` does not currently support node tags as a trigger: [link](https://tickets.corp.opscode.com/browse/OC-7371)

```bash
knife exec -E 'search(:node, "role:opscode-lb* OR role:copsite-lb").each{|n| n.tags << "suicide-ctl"; n.save}'
knife search node 'role:opscode-lb* OR role:corpsite-lb' 'sudo /etc/init.d/chef-client stop'
```

Adding the `suicide-ctl` tag will ensure that if / when the node resumes daemonized chef-client runs (started by cron) that they will exit early and have no effect.

### 3. Knife Upload

* Cookbooks
  * `opscode-lb 1.1.0`
  * `openresty 0.1.8`
* Databags
  * `xdarklaunch`
* Roles
  * opscode-lb
  * opscode-lb-int
  * corpsite-lb

### 4. Resource Planning - Load Balancer Distribution

* rm-366625 - none
* rm-478879 - none
* rm-478864 - lb-rsprod-rv-def10b68
* rm-423003 - lb-rsprod-rv-cc67526b
* rm-478880 - lb-rsprod-rv-8ef1f3d1 (chef restarter)
* rm-478884 - lbcorp-rsprod-rv-085d4ac0
* rm-422842 - lb-rsprod-rv-e0ce9d29
* rm-478881 - lbcorp-rsprod-rv-b5205de2
* rm-478883 - none
* rm-478882 - none
* rm-478885 - lb-rsprod-rv-615143f3

## Deploy - External Test Bake

The Openresty load balancer has been tested in preprod for correctness, but we'd like to get an idea in production of how it (combined with Lua) performs with "real" traffic. The plan is to insert one load openresty load balancer into the roatation alongside the existing nginx load balancers and watch for errors and exceptional resource utilization.

**Note:** `ocvmb` does not currently install Chef from omnibus: [link](https://tickets.corp.opscode.com/browse/OC-7369)

The plan:

1. Create the VM

   On a server without an existing load balancer, use `ocvmb` to create a new external load balancer. ([`ocvmb` docs on wiki](http://wiki.corp.opscode.com/display/CORP/Rackspace+Production#RackspaceProduction-CreatingVMsusing%7B%7Bocvmb%7D%7D))

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

1. Set IP to Static

   For the NAT from the firewall to work properly we need to set the IP address of the new load balancer to be static.

1. Add NAT Rule to ASA Firewall

   Once the ubuntu user has been removed from the load balancer, we can safely connect it to the Internet by adding a NAT rule to the Cisco ASA Firewalls in Rackspace.

   *Nathan, Pauly, or Ian can do this.*

1. Pedant Test the New Load Balancer

   Once the load balancer has been connected to the Internet, we will want to run pedant tests against it to verify that the functionality is correct.

   Edit your `/etc/hosts` file to add the IP of the new load balancer for `api.opscode.com`:

   ```
   # - server ip below - # - hostname below - #
   123.123.123.123       api.opscode.com
   ```

1. Add the load balancer into Dynect DNS:

   Once the correctness of the load balancer has been validated, we can add it into the DNS rotation.

   *Nathan, Pauly, or Ian can do this.*

1. Monitor Errors / Resource Utilization

   While the load balancer is serving prodiction traffic, monitor that there are not an abnormal number of errors coming from this node and that it is not using exceptionally more resources than the other load balancers.

## Deploy - External

Once we're satisfied with performance and correctness of the initial Openresty load balancer, we can replace the Nginx on the remaining nodes with Openresty.

Do the following for each of the Nginx nodes:

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

1. Deploy Openresty

   ```bash
   sudo chef-client
   ```

1. Pedant Test the New Load Balancer

   Once Openresty has been build and configured we will want to run pedant tests against it to verify that the functionality is correct.

   Edit your `/etc/hosts` file to add the IP of the new load balancer for `api.opscode.com`:

   ```
   # - server ip below - # - hostname below - #
   123.123.123.123       api.opscode.com
   ```

1. Add the Node Back Into Dynect DNS

   *Nathan, Pauly, or Ian can do this.*


