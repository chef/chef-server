+++
title = "Runbook"
draft = false

gh_repo = "chef-server"

aliases = ["/runbook/"]

[menu]
  [menu.server]
    title = "Runbook"
    identifier = "server/overview/Runbook"
    parent = "server/overview"
    weight = 50
+++

{{% chef_server %}}

{{% chef_server_component_erchef_background %}}

The following diagram shows the various components that are part of a
Chef Infra Server deployment and how they relate to one another.

<img src="/images/server/server_components_14.svg" width="500" alt="Diagram of Chef Infra Server deployment" />

<table style="width:99%;">
<colgroup>
<col style="width: 12%" />
<col style="width: 87%" />
</colgroup>
<thead>
<tr class="header">
<th>Component</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><p>Bookshelf</p></td>
<td><p>{{< readFile_shortcode file="chef_server_component_bookshelf.md" >}}</p>
<p>All cookbooks are stored in a dedicated repository.</p></td>
</tr>
<tr class="even">
<td>Erchef</td>
<td>{{< readFile_shortcode file="chef_server_component_erchef.md" >}}</td>
</tr>
<tr class="odd">
<td>Messages</td>
<td>
  <p>{{< readFile_shortcode file="chef_server_component_elasticsearch.md" >}}</p>
  <p>All messages are added to a dedicated search index repository.</p>
</td>
</tr>
<tr class="even">
<td>Nginx</td>
<td>{{< readFile_shortcode file="chef_server_component_nginx.md" >}}</td>
</tr>
<tr class="odd">
<td>PostgreSQL</td>
<td>{{< readFile_shortcode file="chef_server_component_postgresql.md" >}}</td>
</tr>
</tbody>
</table>
