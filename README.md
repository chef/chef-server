# CHEF MOVER #

![Migration Visualization](http://images.memegenerator.net/instances/400x/10601974.jpg)

Orchestrate and execute data migration from Ruby-Chef/CouchDB to
Erchef/PgSQL using moser and darklaunch.

## Dev Setup ##

### One-time configuration ###
1. Obtain git checkouts of [this repo][] and the [moser][] repo and ensure they are in the same parent directory.

1. Add an export for an `OPSCODE_PLATFORM_REPO` environment variable to your shell config that points to the path of your **rs-preprod** checkout of the [opscode-platform-cookbooks][] repo.

1. Obtain couchdb data from either preprod or a local dev-vm and put the `.couch` files in a sub-directory of this repo named `moser-couch-data`.

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
1. Start a vm
   ```
   bin/vagrant up
   ```
1. Log in, build release, start mover
   ```
   bin/vagrant ssh
   cd /srv/mover-build
   rebar get-deps
   make devrel
   cp sys.config rel/mover/etc
   cd rel/mover
   bin/mover console
   ```

#### Authz Id Lookup Passthrough Configuration

In order for authz id lookup passthrough to couchdb to work in the dev
VM you will need to ensure that there is a chef_db configuration block
containing entries for ``couch_db_host`` and ``couch_db_port``

Note that you can test without a valid couchdb configuration if you
don't need authz id passthru functionality.

If you're testing locally in the vm instance above, there is no valid couchdb
configuration available.  You can test using preprod as follows assuming
your dev laptop is connected remotely:

* ``bin/vagrant ssh``
* Change /srv/mover-build/rel/mover/etc/sys.config: ``chef_db { couchdb_host = "localhost" }``
* add to (or create) ~/.ssh/config:

        Host *
          ForwardAgent yes

* in the same vagrant ssh session:

        ssh -L 5984:localhost:5984 $YOURUSERNAME@gateway.opscode.com
        ssh -L 5984:localhost:5984 $PREPROD-COUCHDB-HOST

Leave this session open for the duration of your testing.
If you are on-site at HQ you can skip the intermediary tunnel through
gateway.opscode.com

Moser will now be able to connect to preprod couch as needed to look up
authz ids.

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

