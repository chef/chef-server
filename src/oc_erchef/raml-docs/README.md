# RAMLing

RAML is a YAML-based format for REST API documentation. For more about
RAML, see: [raml.org](http://raml.org)

## State of oc_erchef RAML Documentation

Official and complete documentation of the Chef Server API is hosted at
[docs.chef.io](http://docs.chef.io/api_chef_server.html). We find RAML
useful during development of new features, but at this time have no
plans to make these docs official or comprehensive.

## Building HTML Docs From RAML Source

1) swallow your pride and get node.js
2) `npm install -g raml2html`
3) `raml2html base.yml > OUTFILE.html`
4) `open OUTFILE.html`

