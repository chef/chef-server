+++
title = "Infra Server Telemetry"
draft = false

gh_repo = "chef-server"

aliases = ["/server_services.html", "/runbook/telemetry_infra_server/"]

[menu]
  [menu.server]
    title = "Infra Server Telemetry"
    identifier = "server/installation/Infra Server Telemetry"
    parent = "server/installation"
    weight = 80
+++

Chef Infra Server, in accordance with the license agreement, diligently captures license usage data. This process, crucial for the smooth operation of the system, does not require user approval or intervention and runs seamlessly in the background.  

## License Usage

Chef Infra Server will collect the following information from your environment:

* **Version of Infra Server:** The version Chef Server is running on.
* **FQDN of the load balancer Chef Server is installed:** This information, which identifies the API FQDN behind which the Chef Server is placed, is essential for determining if multiple instances of the Chef Server are in use.
* **Customer Name:** This is captured by scanning through the user emails in the Chef Server and processing the email domain.
* The number of nodes checked into the server in the last 30 days.
* **Binary file name and location of Chef Server Ctl command:** This is the Chef Server Ctl command file name, which is used to manage and maintain the Chef Server.
* **Location of Chef Infra Server's configuration file:** This file holds the running configuration of Chef Server.

{{< note >}} Any configuration cannot disable the telemetry feature. {{< note/ >}}
