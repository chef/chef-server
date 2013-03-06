# CHEF MOVER #

![Migration Visualization](http://images.memegenerator.net/instances/400x/10601974.jpg)

Orchestrate and execute data migration from Ruby-Chef/CouchDB to
Erchef/PgSQL using moser and darklaunch.

## Dev Setup ##

### One-time configuration ###

1. Obtain git checkouts of [this repo][] and the [moser][] repo and ensure they
   are in the same parent directory.

2. Add an export for an `OPSCODE_PLATFORM_REPO` environment variable
   to your shell config that points to the path of your **rs-preprod**
   checkout of the [opscode-platform-cookbooks][] repo.

### Running mover in a vm for dev work ###

Assuming you following the above setup instructions, the following
steps should give you a vm with postgres, mover, moser (symlinked into
deps ala dev-vm), and a usable `sys.config` in  `/srv/mover-build`
which you can copy into `rel/mover/etc` once you build a release. The
release is not built because of a quirk of Vagrant provisioning
preventing SSH auth forwarding at provision time (should work fine
once you ssh in). The database will have been initialized with the
opscode_chef schema via chef-sql-schema.

1. Spindle, stub, and mutilate your bundle
   ```
   cd chef-mover
   bundle install --binstubs
   ```
2. Start a vm
   ```
   bin/vagrant up
   ```
3. Log in, build release, start mover
   ```
   bin/vagrant ssh
   cd /srv/mover-build
   rebar get-deps
   make devrel
   cp sys.config rel/mover/etc
   cd rel/mover
   bin/mover console

#### Default file locations for moser ####

The [opscode-chef-mover][] cookbook will configure moser to look in
`/vagrant/moser-data` for account db DETs files and in
`/vargrant/moser-couch-data` for org CouchDB files that are candidates
for migration. These paths correspond to the top-level directory of
your local checkout of chef-mover.

We are using a premade copy of the account DETs files and a
hand-selected number of couch files from our own orgs for local
testing. This minimizes the amount of sensitive data on our vm for
basic dev work.

[moser]: https://githubt.com/opscode/moser
[this repo]: https://githubt.com/opscode/chef-mover
[opscode-platform-cookbooks]: https://githubt.com/opscode/opscode-platform-cookbooks
[opscode-chef-mover]: https://githubt.com/opscode/opscode-chef-mover

