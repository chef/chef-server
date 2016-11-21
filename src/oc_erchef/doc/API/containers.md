Containers and Groups API document
===============================

Purpose
-------

The purpose of this doc is to capture some of the learning we've
accumulated as part of moving containers from opscode-account and
couchdb to erchef and SQL.

Authentication
----------

These API use the same authentication scheme as the rest of chef; see
http://docs.chef.io/api_chef_server.html for a description.

Tasks Remaining
----------

This is derived from reading the opscode-account code. A complete
specification should incorporate more sources.

TODO: Read tests in detail, verify that these are correct, and identify missing tests. In the end
tests should define this spec exactly
TODO: Read erchef implementation, and note differences
TODO: Walk through all version specific annotations and clarify when they apply
TODO: Consider extending this to ACLs
TODO: Write a similar doc for ORGs
TODO: Explain what system containers mean.

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
```
{
	 "containername" : "NAME",
         "containerpath" : "NAME"
}
```

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
```
{
  "clients" : "API_URL/organizations/ORGNAME/containers/clients",
  "containers" : "API_URL/organizations/ORGNAME/containers/containers",
  ...
}
```

##### Response codes

| Code | Response |
| ---  | :-----   |
| 200  | Ok |
| 401  | Unauthorized (TODO check that this is tested) |
| 403  |  The requestor does not have permission. The requestor must have READ permissions in the 'containers' container, and be a member of that organization. |

#### POST

The POST method creates a new container in an organization. It takes no parameters.

##### Request

POST /containers

With a request body of the form

```
{
	 "containername" : "NAME",
         "containerpath" : "NAME"
}
```

The containerpath element is ignored and replaced with the value of the containername element in
SQL, and will be dropped entirely in the future.

TODO: Describe behavior when we fail validation for the structure.

##### Response

The 'Location' header is set to `API_URL/organizations/ORGNAME/containers/NAME`, and the response
body will be 

```{ "uri" : "API_URL/organizations/ORGNAME/containers/NAME" }```

##### Response codes

| Code | Response |
| ---  | :-----   |
| 201  |  Ok |
| 400  |  Bad request if validation fails |
| 401  | Unauthorized (TODO check that this is tested) |
| 403  | Forbidden. The requestor does not have permission. The requestor must have CREATE permissions in the 'containers' container and be a member of the organization.|
| 409  | Conflict: "Container already exists." A bug in opscode-account causes it to return 403 on conflicts. This should be fixed when we
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

```
{
	 "containername" : "NAME",
         "containerpath" : "NAME"
}
```

##### Response codes

| Code | Response |
| ---  | :-----   |
| 200  | Ok |
| 401  | Unauthorized (TODO check that this is tested) |
| 403  | The requestor does not have permission. The requestor must have READ permissions on the ACE for this container and be a member of the organization. |

#### PUT

The PUT method updates a specific container. It takes no parameters, but does take a request body.
(TODO Find out the behavior when the body is omitted)
This API should be removed in the erchef version. Containerpath is being deprecated,

This API allows a put of a different name, which *may* cause a rename or a validation error.

##### Request

PUT /containers/NAME

with a body of the form:
```
{
	 "containername" : "NAME",
         "containerpath" : "NAME"
}
```

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

| Code | Response |
| ---  | :-----   |
| 200  |  Ok |
| 201  | Ok (when a new object was created via rename) |
| 401  | Unauthorized (TODO check that this is tested) |
| 403  | The requestor does not have permission. The requestor must have UPDATE permissions on the ACE for this container and be a member of the organization.  |


#### DELETE

The DELETE method deletes a container. It takes no parameters.

The user can may be able to delete a system container at the present time (VERIFY that this isn't
blocked via ACL); we should fix this.

##### Request

DELETE /containers/NAME

##### Response

The contents of the deleted container:

```
{
	 "containername" : "NAME",
         "containerpath" : "NAME"
}
```

##### Response codes

| Code | Response |
| ---  | :-----   |
| 200  |  Ok |
| 401 | Unauthorized (TODO check that this is tested) |
| 403 |  The requestor does not have permission. The requestor must have DELETE permissions on the ACE for this container and be a member of the organization. |




#### MERB Rules (delete in final version)
s.match("/containers/", :method=>"post").to(:controller=>'containers', :action=>'create')
s.match("/containers/", :method=>"get").to(:controller=>'containers', :action=>'index')
s.match("/containers/:id", :id => /[\w\.-]+/, :method=>"get").to(:controller=>'containers', :action=>'show')
s.match("/containers/:id", :id => /[\w\.-]+/, :method=>"put").to(:controller=>'containers', :action=>'update')
s.match("/containers/:id", :id => /[\w\.-]+/, :method=>"delete").to(:controller=>'containers', :action=>'destroy')

