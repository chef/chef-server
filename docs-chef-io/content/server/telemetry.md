+++
title = "Chef Infra Server telemetry"
draft = false

gh_repo = "chef-server"

[menu]
  [menu.server]
    title = "Telemetry"
    identifier = "server/installation/Infra Server Telemetry"
    parent = "server/installation"
    weight = 80
+++

Chef Infra Server captures license usage data under the Progress Chef license agreement. This process runs in the background, it doesn't require user approval or intervention, and it can't be disabled.

## Collected data

Chef Infra Server collects the following information from your environment:

- The Chef Infra Server version.
- The FQDN of the load balancer that Chef Infra Server is installed on. This information determines if multiple instances of the Chef Infra Server are in use.
- The customer name. Chef Infra Server captures this by scanning email addresses stored on Infra Server and processing the email domain.
- The number of nodes checked into the server in the last 30 days.
- The `chef-server-ctl` binary filename and location.
- The Chef Infra Server's configuration file location.
