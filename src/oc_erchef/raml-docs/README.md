# RAMLing

RAML is a YAML-based format for REST API documentation. For more about
RAML, see: [raml.org](http://raml.org)

## State of oc_erchef RAML Documentation

Official and complete documentation of the Chef Server API is hosted at
[docs.chef.io](http://docs.chef.io/api_chef_server.html). We find RAML
useful during development of new features, but at this time have no
plans to make these docs official or comprehensive.

## Building HTML Docs From RAML Source

### Prerequisites

`raml2html` is a node.js app. You won't be able to generate these docs without it.

### The Makefile

The Makefile should take care of the rest. If you don't have `raml2html`,
it will install it. All you need to do is

```
make
```

Then you'll have an `index.html` file with all these fabulous api docs. An easy
way to view them on Mac OS X would be `open index.html`

If you ever get sick of them, it's nothing you can't solve with `make clean`
