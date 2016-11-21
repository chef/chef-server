# oc-chef-pedant
This is the pedant test suite for Chef Server.

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

## Setting up precommit hooks

It's strongly advised that you link the included pre-commit.sh in as
a precommit hook, in order to prevent accidentally committing code
containing a :focus tag.

To do so:

    $ cd .git/hooks
    $ ln -s ../../pre-commit.sh pre-commit

If you ever need to skip this commit hook ( such as when using :focus in
documentation) you can use:

    $ git commit -n


## Running the Tests in Development

The easiest way to run tests when hacking on either private chef
components or this test suite itself is to load this project into dev VM
and use `chef-server-ctl test`. If you can't do that for whatever
reason, you can run `bin/oc-chef-pedant -c CONFIG`. See
[pedant_config.rb](pedant_config.rb) for an example. Be sure
to correctly configure pedant for Ruby/Erlang as appropriate for your
test system.

When running oc-chef-pedant via `chef-server-ctl test`, pedant uses
configuration in `/var/opt/opscode/oc-chef-pedant/etc/pedant_config.rb`.
HTTP request data is logged to
`/var/log/opscode/oc-chef-pedant/http-traffic.log`.

When running tests manually, logging (and lots of other options) are
configured by command line switches. Run `bin/oc-chef-pedant -h` to list
them.

## Shared Functionality

Where functionality in OPC is shared/identical to that in Open Source
Chef Server, this test suite uses tests from
[chef-pedant](https://github.com/chef/chef-pedant). By default, dev
VM is configured to mount your local chef-pedant checkout into the VM
when loading `oc-chef-pedant`. See the [dev VM config](https://github.com/chef/opscode-dev-vm/blob/master/config/projects.json)
for details.

## Running Non-default Tests

There are some tests that only make sense to run in certain environments or that don't make sense to run by default.

#### LDAP Testing

WARNING: Do not perform this testing on a production environment as it interacts with actual LDAP credentials.

You will need LDAP running on your EC test server with a valid LDAP user. Follow the instructions here to get LDAP running with EC http://docs.chef.io/server_ldap.html.

Please update the entries in `pedant_config.rb`. Set `ldap_testing` to true,
and fill in ldap({}) with your LDAP credentials a user on the test server you pointed EC at above.

This is good enough for quick ad-hoc testing, but we should develop better LDAP integration tests in the future.

#### Account Tests That Talk Via Internal Ports

The opscode-account endpoint for internal org creation and updating (```/internal-organizations```) communicates via the internal account port. That endpoint is not exposed via the external-lb. Therefore, they will fail when kicking off the tests from a point external to the lb (say, developer's laptop hitting hosted for pedant). However, they are useful in validating that these endpoint are still functioning, so if you are running pedant somewhere with access to the internal-lb, simply run

```chef-server-ctl test --all``` or ```private-chef-ctl test --only-internal-orgs```

to execute these tests.

If you want to run all the tests without the internal organization tests run

```chef-server-ctl test --all --exclude-internal-orgs```
