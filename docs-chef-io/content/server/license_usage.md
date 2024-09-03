+++
title = "Chef Infra Server License Usage"
draft = false

gh_repo = "chef-server"

[menu]
  [menu.server]
    title = "License Usage"
    identifier = "server/installation/Infra Server License Usage"
    parent = "server/installation"
    weight = 80
+++

Chef Infra Server captures license usage data under the Progress Chef license agreement. This process runs in the background, it doesn't require user approval or intervention, and it can't be disabled. All information is collected and utilized pursuant to the Progress [Privacy Policy](https://www.progress.com/legal/privacy-policy) which is available through the Progress [Privacy Center](https://www.progress.com/legal/privacy-center).

## Collected data

Chef Infra Server collects the following information from your environment:

- The Chef Infra Server version.
- The domain name of the Chef Infra Server installation and the number of installations by collecting the following parameters:
  - Reports the domain name associated with the most common number of logins registered with Chef Infra Server. For example, `progress.com` or `example.com`.
  - Hashing subdomain details and combining them with the internal load balancer's domain name to prevent transmitting FQDN in its identifiable form but only essential fields to identify unique installations of the Infra Server.
- The number of nodes checked into the server in the last 30 days.
- The `chef-server-ctl` binary filename and location.
- The Chef Infra Server's configuration file location.
