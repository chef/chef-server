# CHEF MOVER #

Orchestrate and execute data migration from Ruby-Chef/CouchDB to
Erchef/PgSQL using moser and darklaunch.

## License

All files in the repository are licensed under the Apache 2.0 license. If any
file is missing the License header it should assume the following is attached;

```
Copyright 2014 Chef Software Inc

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
```

## Dev Setup ##

### One-time configuration ###
1. Ensure that you have vagrant 1.1 or later installed, and that you
   have installed the vagrant-berkshelf and vagrant-omnibus plugins.
1. Obtain git clones of the following projects, and ensure that they're
   in the same parent directory:
   * `git@github.com:opscode/moser.git`
   * `git@github.com:opscode/decouch.git`
   * `git@github.com:opscode/chef-mover.git`
1. Add an export for an `OPSCODE_PLATFORM_REPO` environment variable to your shell
   config that points to the path of your **rs-preprod** checkout of the
   [opscode-platform-cookbooks][] repo.
1. Obtain couchdb data from either preprod or a local dev-vm and put the `.couch`
   files in a sub-directory of this repo named `moser-couch-data`.
1. Your github ssh key must be in your keychain - you can add it via
   ssh-add prior to starting the VM.

### TODO: Out of date upstream changes ###

We are currently pinning to commit `e223e9e1496dcf1ec9e16f3f23b13db122748839` of eredis because
they introduced a breaking change to the API. We should update our code to be compatible with
the new API, but until we have time to do so, make sure to pin to this commit in the lock file
after you update it (via `make prepare_release` or other methods).

### Running mover in a vm for dev work ###

Assuming you following the above setup instructions, the following
steps should give you a vm with postgres, built and configured
mover, with  moser and decouch symlinked into deps ala dev-vm.

The database will have been initialized with the
opscode_chef schema via chef-sql-schema, using schema name
opscode_chef_test.

1. Start the VM

   ```
   cd chef-mover
   vagrant up
   ```
1. Wait for resource "get_mover_deps" to fail because of git auth
failure, then do the following to complete the configuration:

   ```
   vagrant provision
   ```
1. SSH in and start mover

   ```
   bin/vagrant ssh
   cd /srv/mover-build/rel/mover
   bin/mover console
   ```
1. Reference README_FOR_ORGS.md for details around loading and preparing
   the migration state tracking data to allow migrations to occur.

#### Demigrate Testing Configuration
To enable demigration testing, you must have available a properly
configured rabbitmq server.  If present, edit Vagrantfile and set
`"demigrate_override" => true` then re-provision via `vagrant provision`.

#### Authz Id Lookup Passthrough Configuration

In order for authz id lookup passthrough to couchdb to work in the dev
VM you will need to ensure that there is a chef_db configuration block
containing entries for `couch_db_host` and `couch_db_port`

Note that you can test without a valid couchdb configuration if you
don't need authz id passthru functionality.

If you're testing locally in the vm instance above, there is no valid couchdb
configuration available.  You can test using preprod as follows assuming
your dev laptop is connected remotely:

* `bin/vagrant ssh`
* add to (or create) ~/.ssh/config:

        Host *
          ForwardAgent yes

* in the same vagrant ssh session:

        ssh -L 5984:localhost:5984 $YOURUSERNAME@gateway.chef.io

* then once connected:

        ssh -L 5984:localhost:5984 $PREPROD-COUCHDB-HOST

Leave this session open for the duration of your testing.
If you are on-site at HQ you can skip the intermediary tunnel through
gateway.chef.io

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


