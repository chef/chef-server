Groups API document
===============================

Purpose
-------

The purpose of this doc is to capture some of the learning we've accumulated as part of moving
groups from opscode-account and couchdb to erchef and SQL.

Authentication
----------

This API uses the same authentication scheme as the rest of chef; see
http://docs.chef.io/api_chef_server.html for a description.


Tasks Remaining
-------

This is derived from reading the opscode-account code. A complete specification should incorporate
more sources.

TODO: Read tests in detail, verify that these are correct, and identify missing tests. In the end
tests should define this spec exactly
TODO: Read erchef implementation, and note differences
TODO: Walk through all version specific annotations and clarify when they apply
TODO: Explain what system groups and containers mean.

Groups
------

Groups provide abstraction and indirection to the authorization system. Users can create first class
groups, but there are some special system groups that all organizations must have. These are:
"users","clients","admins", "billing-admins". (TODO Document the function of each of these groups)

Some special services (reporting and pushy) use other special groups as a form of access
control. (TODO document more)

There also is a global group 'ORGNAME_read_access_group'. This may be deprecated in future versions, and
is for internal use only.

(TODO: Explain exactly what this group does)

The actual group JSON object is:
```
{
	 "actors" => ["user1", "user2", "client1", "client2"]
         "users" => ["user1", "user2"]
	 "clients" => ["client1", "client2"]
	 "groups" => ["group1", "group2"],
	 "orgname" => "ORGNAME",
         "name" => "A_GROUP_NAME",
	 "groupname" => "A_GROUP_NAME"
}
```

Note that each user is listed twice, once as part of the 'actors' field, and once in the users
field. Clients are also listed twice. The 'users' and 'clients' fields are read only, and are
provided for the convenience of upstream APIs.

'actors' is the list of actors (users and clients) in the group.
'groups' is the list of groups in the group.

'orgname' is the name of the organization and is a read only field.

'name' is the name of the group, and can be reset (TODO verify). 'groupname' is a alias for
'name'. Name must match the regex '^[a-z0-9\-_]+$'. (TODO verify that this is consisteint in erchef
and in nginx routing)

### Resource /groups

The groups resource has two methods, GET and POST

#### GET

The GET method returns the list of groups for an org. It does not list the ORGNAME_read_access_group
group. This method takes no parameters

##### Request

GET /groups

##### Response


The response body will be something like the following:
```
{
	 "actors" => ["user1", "user2", "client1", "client2"]
         "users" => ["user1", "user2"]
	 "clients" => ["client1", "client2"]
	 "groups" => ["group1", "group2"],
	 "orgname" => "ORGNAME",
         "name" => "A_GROUP_NAME",
	 "groupname" => "A_GROUP_NAME"
}
```
##### Response

The response body will be something like the following:
```
{
	"admins" => "API_URL/organizations/ORGNAME/groups/admins",
	"billing-admins" => "API_URL/organizations/ORGNAME/groups/billing-admins",
	"clients" => "API_URL/organizations/ORGNAME/groups/clients",
	"users" => "API_URL/organizations/ORGNAME/groups/users",
}
```

They will not be returned in any particular order.

##### Response codes

| Code | Response |
| ---  | :-----   |
| 200 | Ok |
| 401 | Unauthorized (TODO check that this is tested) |
| 403 | The requestor does not have permission. The requestor must have READ permissions in the 'groups' container and be a member of the organization. |

#### POST

The POST method creates a new group in an organization. It takes no parameters.

##### Request

POST /groups

With a request body of the form

```
{
	 "id" => "A_GROUP_NAME"
}
```

All other fields are ignored. Specifically, we ignore actor and group fields.

'groupname' can also be used to set the name, but 'id' wins if both are set. The 'name' field is ignored.

TODO extend tests to exercise actors/groups better; we don't test POST of groups with actors and
groups. We should extend the API to allow us to create a group with actor and groups defined.

TODO describe validation failure behavior better.

TODO BUG new ruby-sql groups api actually appears to accept and set actors

##### Response

The response body will be

```{ "uri" : "API_URL/organizations/ORGNAME/groups/NAME" }```

Unlike containers the 'Location' header is not set. This is probably a bug.

##### Response codes

| Code | Response |
| ---  | :-----   |
| 201 | Ok |
| 400 | Bad Request if validation fails |
| 401 | Unauthorized (TODO check that this is tested) |
| 403 | Forbidden. The requestor does not have permission. The requestor must have CREATE
permissions in the 'groups' container and be a member of the organization. |
| 409 | Conflict: "Container already exists." |

### Resource /groups/NAME

This operates on individual groups.

#### GET

The GET method retrieves a group. It takes no parameters.

#####  Request

GET /groups/NAME

##### Response

The contents of the group. This includes authz related information
about group membership.

`
{
	 "actors" => ["user1", "user2", "client1", "client2"]
         "users" => ["user1", "user2"]
	 "clients" => ["client1", "client2"]
	 "groups" => ["group1", "group2"],
	 "orgname" => "ORGNAME",
         "name" => "A_GROUP_NAME",
	 "groupname" => "A_GROUP_NAME"
}
`

##### Response Codes

| Code | Response |
| ---  | :-----   |
| 200 | Ok |
| 401 | Unauthorized (TODO check that this is tested) |
| 403 | The requestor does not have permission. The requestor must have READ permissions on the ACE for this group and be a member of the organization. |

#### PUT

The PUT method updates a specific group. It takes no parameters, but
does take a JSON request body. The API allows the name of the group in
JSON to differ from that in the resource and does a rename.

##### Request

PUT /groups/NAME

The JSON expected is  a bit different than what is returned via
GET. Membership information is contained in an 'actors' field instead
of being flattened as it is returned by GET.

`
{
 "groupname" : "A_GROUP_NAME",
 "actors" : {"clients" :  ["CLIENT1", "CLIENT2" ...],
             "users" : ["USER1", "USER2" ...],
             "groups" : ["GROUP1", "GROUP2" ...]},
 "orgname" :"ORGNAME"
}
`

The request body may have the following fields:

* 'groupname': If the groupname differers from the NAME provided, this
is a rename operation.
* 'orgname': may be omitted.
* 'actors'; which has subfields
** 'clients'
** 'users'
** 'groups'

If any subfield is omitted, it is treated as if it were the empty list.

Trying to set orgname to a different value raises a 400.

##### Response Codes

| Code | Response |
| ---  | :-----   |
| 200 | Ok |
| 201 | Ok (when a new object was created via rename) |
| 400 | Validation falures of the JSON blob, including attempting to set a read only field |
| 401 | Unauthorized (TODO check that this is tested) |
| 403 | The requestor does not have permission. The requestor must have UPDATE permissions on the ACE for this container and be a member of the organization. |
| 409 | If the new groupname is of a group that already exists. |

#### DELETE

TODO: Can we delete a system group? Should this be possible?

The DELETE method deletes a group. It takes no parameters.

The user can may be able to delete a system group at the present time
(VERIFY that this isn't blocked via ACL); we should fix this.

##### Request

DELETE /group/NAME

##### Response

The response body will be the group as it was before deletion.

`
{
	 "actors" => ["user1", "user2", "client1", "client2"]
         "users" => ["user1", "user2"]
	 "clients" => ["client1", "client2"]
	 "groups" => ["group1", "group2"],
	 "orgname" => "ORGNAME",
         "name" => "A_GROUP_NAME",
	 "groupname" => "A_GROUP_NAME"
}
`


##### Response codes

| Code | Response |
| ---  | :-----   |
| 200 | Ok |
| 401 | Unauthorized (TODO check that this is tested) |
| 403 | The requestor does not have permission. The requestor must have DELETE permissions on the ACE for this group and be a member of the organization. |



#### MERB Rules (delete in final version)
s.match("/groups", :method=>"post").to(:controller=>'groups', :action=>'create')
s.match("/groups", :method=>"get").to(:controller=>'groups', :action=>'index')
s.match("/groups/:id", :method=>"get").to(:controller=>'groups', :action=>'show')
s.match("/groups/:id", :method=>"put").to(:controller=>'groups', :action=>'update')
s.match("/groups/:id", :method=>"delete").to(:controller=>'groups', :action=>'destroy')
