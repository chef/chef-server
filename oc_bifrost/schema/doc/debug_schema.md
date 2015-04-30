Debug Schema
============

There are some helpful views defined in the `debug` schema of the
Bifrost database that may be useful for inspecting the system.  These
are intended to help both support and on-call engineers by providing a
simpler query interface with a unified conceptual view of the system,
without requiring intimate knowledge of the underlying data model.

These are not used in the implementation of Bifrost, and are not exposed
via REST or any other interface; all interaction is via the `psql`
shell.

In addition to these READMEs, some documentation is available in
`psql` itself, in the form of comments.  To see them, use the `\d+`
directive; for example, to see the comments on the `object_acl` view:

```
bifrost=# \d+ debug.object_acl
                                                    View "debug.object_acl"
      Column      |      Type       | Modifiers | Storage  |                            Description
------------------+-----------------+-----------+----------+-------------------------------------------------------------------
 object           | character(32)   |           | extended | The Authz ID of an object
 authorizee       | character(32)   |           | extended | The Authz ID of an actor or group with a permission on `object`
 type             | text            |           | extended | Either "actor" or "group"; refers to `authorizee`
 permission       | auth_permission |           | plain    | The permission `authorizee` has on `object`
 directly_granted | boolean         |           | plain    | Is `permission` directly granted, or present by group membership?

...
```

# Views

* [debug.object_acl](object_acl.md)
