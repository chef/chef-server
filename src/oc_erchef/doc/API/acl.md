ACLs API document
==========================

Purpose
-------

The purpose of this doc is to capture some of the learning we've accumulated as part of moving
groups from opscode-account and couchdb to erchef and SQL.

Authentication
----------

This API uses the same authentication scheme as the rest of chef; see
http://docs.chef.io/api_chef_server.html for a description.

Tasks Remaining
--------------

TODO: Formalize API endpoint doc


Rough description of API.
---------------

The ACLs endpoint can be understood as two APIs replicated across
three different object types; this generates resources of the form
'object\_resource/'api\_endpoint'. A regex for it might look like:

(organizations/:orgname/:type/:object_name |
 organizations/:orgname/:object |
 users/:username ) /
 (\_acl|\_acl/(create|read|update|delete|grant))

These object resources can be one of three types:

1. Scoped by org

Objects scoped by org are all the normal types. Here the resource
starts with organizations/:orgname/:type/:object_name. Examples might
be "organizations/exampleorg/containers/nodes" or
"organizations/whizzoproducts/nodes/my\_node". Orgname and object
names should be restricted to be the same what as the various object
endpoints enforce; if we can create an object with that name, we
should be able to edit its ACL.

2. Organizations

Organization acls are accessed similarly to other org scoped objects,
except that the object name is omitted:
organizations/:orgname/:object. An example would be
"organizations/whizzoproducts/organization". The :object field
apparently is expected to be 'organization' but that doesn't appear to
be checked in the API.

3. Users

User objects are accessed via the user resource,
e.g. "users/invaderzim"

The two APIs available for these types

#### GET _acl api

Appended to the end of the appropriate path for the object type this returns the full ACL for an object.

This is returned as a set of ACEs for each permission type (create,
read, update, delete, grant, aka CRUDG). 
{ "create" : {"actors" : [ "user1" ...],
              "groups" : [ "group1" ... ] } ,
  "read" : ... ,
  "update" : ... ,
  "delete" : ... ,
  "grant" : ... } 

The actors section contains both the _users_ and _clients_ by
name. (NOTE: determine what happens when there is a user and client
with the same name). The groups section contains the groups by
name. In general this will be limited to org scoped groups. The only
known exception is the acl for a user will contain the
ORGNAME\_global\_admins group for any org of which it is a
member. This will complicate the logic to translate from authz\_ids to
names.

The authorization service manages the permissions for who can read an
ACL; at this time if you have any of CRUDG on the object you can read
its ACL.


#### PUT _acl/:ace_type

Appended to the end of the appropriate path for the object type this
sets the ACE specified by ACE type.

It expects JSON of the form:
{"actors" : [ "user1" ...],
 "groups" : [ "group1" ... ] }

The actors and groups are in the same format as returned in the GET
above. 

The authorization service manages the permissions for who can write an
ACL; at this time we require 'grant' permission in the ACL to update
it. 

Implementation Notes
----------------

The actual flow for the three different object types handled by the
API is very similar, and the code should be easy to share between
them. Org scoped objects check if the requester is a member of the
org, while users and orgs do not (verify that orgs do not, we don't
test that)

The groups endpoint should already have code for mapping actor and
group names to authz_ids and the reverse. The biggest exception will
be the ORGNAME\_global\_admins group, which doesn't currently have
mapping support available. The validation code for
json input should be very similar.

It probably makes the most sense to start with the organization scoped object
endpoint and extend the code to handle users and organizations. Direct
ACL read and write for users and organizations are rare, so this
delivers most of the functionality early.

The access policy (who can read/who can update) is determined in the
bifrost\_wm\_acl\_action\_resource:auth_info function. Note that this
resource also allows 'delete' which is not exposed to the end users.


