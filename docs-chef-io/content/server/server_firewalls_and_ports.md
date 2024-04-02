+++
title = "Firewalls and Ports"
draft = false

gh_repo = "chef-server"

aliases = ["/server_firewalls_and_ports.html", "/runbook/server_firewalls_and_ports/"]

[menu]
  [menu.server]
    title = "Firewalls & Ports"
    identifier = "server/reference/Firewalls & Ports"
    parent = "server/reference"
    weight = 40
+++

{{< readfile file="content/server/reusable/md/server_firewalls_and_ports_summary.md" >}}

{{< readfile file="content/server/reusable/md/server_firewalls_and_ports_listening.md" >}}

{{< note >}}

An "external" port is external from the perspective of a workstation
(such as knife), a machine (Chef Infra Client), or any other user that
accesses the Chef Infra Server via the Chef Infra Server API.

{{< /note >}}

## Standalone

The following sections describe the ports that are required by the Chef
Infra Server in a standalone configuration:

![image](/images/server/chef_server_ports_standalone.png)

{{< readfile file="content/server/reusable/md/server_firewalls_and_ports_loopback.md" >}}

For a standalone installation, ensure that ports marked as external
(marked as `yes` in the **External** column) are open and accessible via
any firewalls that are in use:

<table>
<colgroup>
<col style="width: 11%" />
<col style="width: 77%" />
<col style="width: 11%" />
</colgroup>
<thead>
<tr class="header">
<th>Port</th>
<th>Service Name, Description</th>
<th>External</th>
</tr>
</thead>
<tbody>
<tr>
<td><p>4321</p></td>
<td><p><strong>bookshelf</strong></p>
<p>{{< readfile file="content/server/reusable/md/server_services_bookshelf.md" >}}</p></td>
<td><p>no</p></td>
</tr>
<tr>
<td><p>80, 443, 9683</p></td>
<td><p><strong>nginx</strong></p>
<p>{{< readfile file="content/server/reusable/md/server_services_nginx.md" >}}</p>
{{< note >}}
<p>Port 9683 is used to internally load balance the <strong>oc_bifrost</strong> service.</p>
{{< /note >}}</td>
<td><p>yes</p></td>
</tr>
<tr>
<td><p>9463</p></td>
<td><p><strong>oc_bifrost</strong></p>
<p>{{< readfile file="content/server/reusable/md/server_services_bifrost.md" >}}</p></td>
<td></td>
</tr>
<tr>
<td><p>9090</p></td>
<td><p><strong>oc-id</strong></p>
<p>{{< readfile file="content/server/reusable/md/server_services_oc_id.md" >}}</p></td>
<td></td>
</tr>
<tr>
<td><p>8000</p></td>
<td><p><strong>opscode-erchef</strong></p>
<p>{{< readfile file="content/server/reusable/md/server_services_erchef.md" >}}</p></td>
<td></td>
</tr>
<tr>
<td><p>5432</p></td>
<td><p><strong>postgresql</strong></p>
<p>{{< readfile file="content/server/reusable/md/server_services_postgresql.md" >}}</p></td>
<td></td>
</tr>
<tr>
<td><p>9200</p></td>
<td><p><strong>elasticsearch</strong></p>
<p>{{< readfile file="content/server/reusable/md/server_services_elasticsearch.md" >}}</p></td>
<td></td>
</tr>
<tr>
<td><p>16379</p></td>
<td><p><strong>redis_lb</strong></p>
<p>{{< readfile file="content/server/reusable/md/server_services_redis.md" >}}</p></td>
<td></td>
</tr>
</tbody>
</table>

## Tiered

The following sections describe the ports that are required by the Chef
Infra Server in a tiered configuration:

![image](/images/server/chef_server_ports_tiered.png)

{{< readfile file="content/server/reusable/md/server_firewalls_and_ports_loopback.md" >}}

### Front End

{{< readfile file="content/server/reusable/md/server_firewalls_and_ports_fe.md" >}}

### Back End

{{< readfile file="content/server/reusable/md/server_firewalls_and_ports_tiered.md" >}}
