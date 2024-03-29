# Chef Infra Server Addons

This directory exists to allow addons to Chef Infra Server to add upstreams and routing rules to the nginx configuration

## Included files

There are includes in the main Nginx configuration that match the following patterns:

* `*_upstreams.conf`: Upstream definitions
* `*_external.conf`: Routing rules for routes exposed via the external LB
* `*_internal.conf`: Routing rules for routes exposed via the internal LB

## File Naming

Files are included in normal alphabetic ordering.  To ensure consistent ordering, each addon should use a 2 digit numeric prefix followed by the component name.  e.g for addon 'foo' we would have files named `10-foo_upstreams.conf`, `10-foo_external.conf`, etc.
