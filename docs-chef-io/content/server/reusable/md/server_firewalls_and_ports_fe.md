For front-end servers, ensure that ports marked as external (marked as
`yes` in the **External** column) are open and accessible using any
firewalls that are in use:

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
<td><p>80, 443, 9683</p></td>
<td><p><strong>nginx</strong></p>
<p>{{< readfile file="content/server/reusable/md/server_services_nginx.md" >}}</p>
<div class="admonition-note">
<p class="admonition-note-title">Note</p>
<div class="admonition-note-text">

<p>Port 9683 is used to internally load balance the <strong>oc_bifrost</strong> service.</p>

</div>
</div>
</td>
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
<p>{{< readfile file="content/server/reusable/md/server_services_oc_id.md" >}}</p>
</td>
<td></td>
</tr>
<tr>
<td><p>8000</p></td>
<td><p><strong>opscode-erchef</strong></p>
<p>{{< readfile file="content/server/reusable/md/server_services_erchef.md" >}}</p></td>
<td></td>
</tr>
</tbody>
</table>
