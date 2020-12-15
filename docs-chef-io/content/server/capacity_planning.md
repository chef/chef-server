+++
title = "Capacity Planning"

date = 2020-12-15T17:26:28-08:00
draft = false

gh_repo = "chef-server"

[menu]
  [menu.server]
    title = "Capacity Planning"
    identifier = "server/planning/Capacity Planning"
    parent = "server/planning"
    weight = 40
+++

This document provides guidance for capacity planning and how to choose
the right configuration--standalone, high availability, or tiered--for
the Chef Infra Server. This document provides guidance and not hard/fast
rules. This is because some requests to the Chef Infra Server API are
more computationally expensive than others. In general, it's better to
start small and then scale the Chef Infra Server as needed. Premature
optimization can hinder more than help because it may introduce
unnecessary complexity.

## Scaling the Chef Infra Server

The Chef Infra Server itself is highly scalable. A single virtual
machine running the Chef Infra Server can handle requests for many
thousands of nodes. As the scale increases, it's a straightforward
process to expand into a tiered front-end, back-end architecture with
horizontally scaled front-ends to relieve pressure on system
bottlenecks.

That said, it's best to isolate failure domains with their own Chef
Infra Server, rather than trying to run every node in an infrastructure
from a single central, monolithic Chef Infra Server instance/cluster.

For instance, if there are West coast and East coast data centers, it is
best to have one Chef Infra Server instance in each datacenter. Deploys
to each Chef Infra Server can be synchronized upstream by CI software.
The primary limiting bottleneck for Chef Infra Server installations is
almost always input/output operations per second (IOPS) performance for
the database filesystem.

## CCRs/min

The key unit of measure for scaling the Chef Infra Server is the number
of Chef Infra Client runs per minute: CCRs/min. For example, 500 nodes
set to check in every 30 minutes is equivalent to 16.66 CCRs/min.

Typically, the Chef Infra Server does not require a high availability or
tiered topology until the number of CCRs/min is higher than 333/min
(approximately 10k nodes).

While synthetic benchmarks should be taken with a grain of salt, as they
don't typically represent real-world performance, internal synthetic
benchmarks at Chef have seen a standalone Chef Infra Server installed on
a `c3.2xlarge` Amazon Web Services (AWS) instance handle more than 1,000
CCRs/min (30k nodes).

## Assumptions

Several factors may influence server scalability. All server sizing
recommendations are based on these assumptions:

-   Chef Infra Client runs are daemonized, and are not initiated by a
    cron job. Using cron to schedule runs can create "thundering herd"
    problems
-   Chef Infra Client runs are set to a default 30-minute interval with
    a 5-minute splay
-   Search and `partial_search` are utilized, but not heavily
-   The number of cookbooks per organization, across all versions, on
    the Chef Infra Server is under 500. (Multiple organizations with
    fewer than 500 cookbooks each, that still add up to a total of more
    than 500 cookbooks server-wide, is fine.)
-   The default maximum allowable size for a node object is 1MB,
    although it is rare for nodes to exceed 150KB. Though compressed,
    this data is replicated twice, once in Elasticsearch, and once in
    PostgreSQL. In practice, allowing a conservative 2MB of storage on
    the disk partition per node should be sufficient

## Host Specifications

The following sections describe the host specifications for various
sizes of CCRs/min and help show when to consider moving from a
standalone topology to a high availability or tiered topology.

**UP TO 33 CCRs/Min (approx. 1,000 nodes):**

-   Chef recommends a single virtual machine instance
-   Start with 2 CPU cores and 8GB of RAM, which is equivalent to an
    Amazon EC2 `m3.large` instance
-   Allocate 2MB of disk space on the data partition per managed node

**UP TO 167 CCRs/Min (approx. 5,000 nodes):**

-   Chef recommends a single virtual machine instance
-   Start with 4 CPU cores and 16GB of RAM, which is equivalent to an
    Amazon EC2 `m3.xlarge` instance

**UP TO 333 CCRs/Min (Approx. 10,000 nodes):**

-   Chef recommends a single virtual machine instance
-   Start with 8 CPU cores and 32GB of RAM, which is equivalent to an
    Amazon EC2 `m3.2xlarge` instance

**UP TO 667 CCRs/Min (Approx. 20,000 nodes):**

-   Chef recommends two hosts, one front-end and one back-end
-   The disk requirement for the front-end server is negligible
-   Start with 8 CPU cores and 32GB of RAM for each host, which is
    equivalent to an Amazon EC2 `m3.2xlarge` instance

**Scaling beyond 20,000 nodes on a single cluster:**

-   Additional capacity can be gained by placing the front-end node
    behind an HTTP load balancer, and then scaling front-end nodes
    horizontally
-   Chef recommends that Chef professional services be engaged to help
    with capacity and architectural planning at this size