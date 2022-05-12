All machines in a Chef Infra Server deployment have the following
hardware requirements. Disk space for standalone and backend servers
should scale up with the number of nodes that the servers are managing.
A good rule to follow is to allocate 2 MB for each node. The disk values
listed below should be a good default value that you will want to modify
later if/when your node count grows. Fast, redundant storage
(SSD/RAID-based solution either on-prem or in a cloud environment) is
preferred.

**All Deployments**

- 64-bit CPU architecture
- CPU support for SSE4.2 extensions (Xeons starting in 2007 and Opterons in 2012)

**Standalone Deployments**

- 4 total cores (physical or virtual)
- 8 GB of RAM or more
- 5 GB of free disk space in `/opt`
- 10 GB of free disk space in `/var`

For a high availability deployment:

**General Requirements**

- Three backend servers; as many frontend servers as required
- 1 x GigE NIC interface (if on premises)

{{ readFile "layouts/shortcodes/chef-server/system_requirements_ha.md" | markdownify }}
