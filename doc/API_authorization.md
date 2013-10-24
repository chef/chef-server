Containers and Groups API document
===============================

Purpose
-------

The purpose of this doc is to capture some of the learning we've accumulated as part of moving containers and
groups from opscode-account and couchdb to erchef and SQL, and to serve as the nucleus of an API
document for these endpoints.

Authentication
----------

These API use the same authentication scheme as the rest of chef; see
http://docs.opscode.com/api_chef_server.html for a description.

WIP

This is derived from reading the opscode-account code. A complete specification should incorporate
more sources. 

TODO: Read tests in detail, verify that these are correct, and identify missing tests. In the end
tests should define this spec exactly
TODO: Read erchef implementation, and note differences
TODO: Walk through all version specific annotations and clarify when they apply
TODO: Consider extending this to ACLs
TODO: Write a similar doc for ORGs
TODO: Explain what system groups and containers mean.

Containers
----------

Containers serve two conceptually separate purposes. First, they serve as templates for creating
an entity in authz; they are a way of providing a default set of permissions for a newly created
object. Second, they serve as the source of the ACL used to authorize LIST and CREATE operations on
objects.

There's a container for every chef object type that has an ACL. At the present time these are: 
'clients', 'containers, 'cookbooks',  'data', 'environments', 'groups', 'nodes', 'roles',
'sandboxes'.
These may be extended in the future. These are specific to an org, and each org as it's own
copies. These are created when an org is created, and required for an org to function properly.

There are also two global containers 'users' and 'organizations', used to regulate who can create
and list these entities. 

The actual container JSON object is very minimal:
`
{
	 "containername" : "NAME",
         "containerpath" : "NAME"
}
`

containername is the name of the container, and must be a valid ASCII identifier.  The exact name
pattern is a bit conflicted. The routing for opscode-account merb restricts it to
`^[\w|\-\.]+$`. However other internal validators are a bit more relaxed, matching the regex pattern
`^[a-zA-Z0-9\-_\.]*$`. (TODO reconcile this, with nginx routing and erchef port). It appears that we
might be able to create via POST a container that we could not delete because of this rule. (TODO
test this)

containerpath is deprecated, and in future versions of the API will be removed. Currently it is
defined to be exactly the same as containername, and attempts to change it will be ignored.

#### Quirks of the API
We presently allow deletion of system containers. The only check that we perfom is to check the
container ACL for delete permission. However admins must have delete permission on the container ACL
because we use the system container as the template to create new objects, and we want new objects
to be deleteable by admins.

### Resource /containers

The /containers endpoint has the following methods: GET and POST

#### GET

The GET method returns the list of containers for an org. It does not return the global
containers. This method takes no parameters.

##### Request

GET /containers

##### Response

The response body will be something like the following:
`
{
  "clients" : "API_URL/organizations/ORGNAME/containers/clients",
  "containers" : "API_URL/organizations/ORGNAME/containers/containers",
  ...
}
`

##### Response codes

200   Ok
401   Unauthorized (TODO check that this is tested)
403   The requestor does not have permission. The requestor must have READ permissions in the
'containers' container, and be a member of that organization.

#### POST

The POST method creates a new container in an organization. It takes no parameters.

##### Request

POST /containers

With a request body of the form

`
{
	 "containername" : "NAME",
         "containerpath" : "NAME"
}
`

The containerpath element is ignored and replaced with the value of the containername element in
SQL, and will be dropped entirely in the future.

TODO: Describe behavior when we fail validation for the structure.

##### Response

The 'Location' header is set to `API_URL/organizations/ORGNAME/containers/NAME`, and the response
body will be 

`{ "uri" : "API_URL/organizations/ORGNAME/containers/NAME" }`

##### Response codes

201   Ok
400   Bad request if validation fails
401   Unauthorized (TODO check that this is tested)
403   Forbidden. The requestor does not have permission. The requestor must have CREATE permissions in the
'containers' container and be a member of the organization.
409   Conflict: "Container already exists."
      A bug in opscode-account causes it to return 403 on conflicts. This should be fixed when we
      port to erchef.

### Resource /containers/NAME

This operates on individual containers. The rationale for this API is somewhat questionable in that
the system created containers probably should not be modified, and non-system containers don't have
much use in that they're not wired into the system. In future versions we probably want to restrict
deletion of system containers, for example. Also, containers are really just a way of naming a set
of authz acls, and most of the action will be done via the ACL resource.

#### GET

The GET method retrieves a container. It takes no parameters. 

Since the 'containerpath' field is in the process of being deprecated, it effectively returns no
useful information beyond the name, which the requestor presumably already knows.

##### Request

GET /containers/NAME

##### Response

The contents of the container.

`
{
	 "containername" : "NAME",
         "containerpath" : "NAME"
}
`

##### Response codes

200   Ok
401   Unauthorized (TODO check that this is tested)
403   The requestor does not have permission. The requestor must have READ permissions on the ACE
for this container and be a member of the organization.

#### PUT

The PUT method updates a specific container. It takes no parameters, but does take a request body.
(TODO Find out the behavior when the body is omitted)
This API should be removed in the erchef version. Containerpath is being deprecated,

This API allows a put of a different name, which *may* cause a rename or a validation error.

##### Request

PUT /containers/NAME

with a body of the form:
`
{
	 "containername" : "NAME",
         "containerpath" : "NAME"
}
`

The behaviour varies widely between implementations.
In the opscode-account couchdb implementation, missing elements create a structure that fails deeply
in model validation, and returns BadRequest. Containerpath can be updated and the value is
perserved.

In the opscode-account sql implementation, only containername can be changed; alterations to
containerpath are ignored. 
TODO: test for SQL implmentations should verify we rename the object as opposed to creating a new
one (probably with the same authz id).

This API is not supported in the erchef-sql implementation.

##### Response codes

200   Ok
201   Ok (when a new object was created via rename)
401   Unauthorized (TODO check that this is tested)
403   The requestor does not have permission. The requestor must have UPDATE permissions on the ACE
for this container and be a member of the organization.


#### DELETE

The DELETE method deletes a container. It takes no parameters.

The user can may be able to delete a system container at the present time (VERIFY that this isn't
blocked via ACL); we should fix this.

##### Request

DELETE /containers/NAME

##### Response

The contents of the deleted container:

`
{
	 "containername" : "NAME",
         "containerpath" : "NAME"
}
`

##### Response codes

200   Ok
401   Unauthorized (TODO check that this is tested)
403   The requestor does not have permission. The requestor must have DELETE permissions on the ACE
for this container and be a member of the organization.




#### MERB Rules (delete in final version)
s.match("/containers/", :method=>"post").to(:controller=>'containers', :action=>'create')
s.match("/containers/", :method=>"get").to(:controller=>'containers', :action=>'index')
s.match("/containers/:id", :id => /[\w\.-]+/, :method=>"get").to(:controller=>'containers', :action=>'show')
s.match("/containers/:id", :id => /[\w\.-]+/, :method=>"put").to(:controller=>'containers', :action=>'update')
s.match("/containers/:id", :id => /[\w\.-]+/, :method=>"delete").to(:controller=>'containers', :action=>'destroy')


Groups
------

Groups provide abstraction and indirection to the authorization system. Users can create first class
groups, but there are some special system groups that all organizations must have. These are:
"users","clients","admins", "billing-admins". (TODO Document the function of each of these groups)

Some special services (reporting and pushy) use other special groups as a form of access
control. (TODO document more)

There also is a global group 'ORGNAME_global_admins'. This may be deprecated in future versions, and
is for internal use only.

(TODO: Explain exactly what this group does)

The actual group JSON object is:
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

The GET method returns the list of groups for an org. It does not list the ORGNAME_global_admins
group. This method takes no parameters

##### Request

GET /groups

##### Response


The response body will be something like the following:
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
##### Response

The response body will be something like the following:
`
{
	"admins" => "API_URL/organizations/ORGNAME/groups/admins",
	"billing-admins" => "API_URL/organizations/ORGNAME/groups/billing-admins",
	"clients" => "API_URL/organizations/ORGNAME/groups/clients",
	"users" => "API_URL/organizations/ORGNAME/groups/users",
}
`

They will not be returned in any particular order.

##### Response codes

200   Ok
401   Unauthorized (TODO check that this is tested)
403   The requestor does not have permission. The requestor must have READ permissions in the
'groups' container and be a member of the organization.

#### POST

The POST method creates a new group in an organization. It takes no parameters.

##### Request

POST /groups

With a request body of the form

`
{
	 "id" => "A_GROUP_NAME"
}
`

All other fields are ignored. Specifically, we ignore actor and group fields. 

'groupname' can also be used to set the name, but 'id' wins if both are set. The 'name' field is ignored.

TODO extend tests to exercise actors/groups better; we don't test POST of groups with actors and
groups. We should extend the API to allow us to create a group with actor and groups defined.

TODO describe validation failure behavior better.

TODO BUG new ruby-sql groups api actually appears to accept and set actors

##### Response

The response body will be 

`{ "uri" : "API_URL/organizations/ORGNAME/groups/NAME" }`

Unlike containers the 'Location' header is not set. This is probably a bug.

##### Response codes

201   Ok
400   Bad Request if validation fails
401   Unauthorized (TODO check that this is tested)
403   Forbidden. The requestor does not have permission. The requestor must have CREATE permissions in the
'groups' container and be a member of the organization.
409   Conflict: "Container already exists."

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

200   Ok
401   Unauthorized (TODO check that this is tested)
403   The requestor does not have permission. The requestor must have READ permissions on the ACE
for this group and be a member of the organization.

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

200   Ok
201   Ok (when a new object was created via rename)
400   Validation falures of the JSON blob, including attempting to set a read only field
401   Unauthorized (TODO check that this is tested)
403   The requestor does not have permission. The requestor must have UPDATE permissions on the ACE
for this container and be a member of the organization.
409   If the new groupname is of a group that already exists.

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

200   Ok
401   Unauthorized (TODO check that this is tested)
403   The requestor does not have permission. The requestor must have DELETE permissions on the ACE for this group and be a member of the organization.



#### MERB Rules (delete in final version)
s.match("/groups", :method=>"post").to(:controller=>'groups', :action=>'create')
s.match("/groups", :method=>"get").to(:controller=>'groups', :action=>'index')
s.match("/groups/:id", :method=>"get").to(:controller=>'groups', :action=>'show')
s.match("/groups/:id", :method=>"put").to(:controller=>'groups', :action=>'update')
s.match("/groups/:id", :method=>"delete").to(:controller=>'groups', :action=>'destroy')




