# oc-chef-pedant

**Internal Repo**

This is the pedant test suite for Opscode Private Chef.

## Running the Tests in Development

The easiest way to run tests when hacking on either private chef
components or this test suite itself is to load this project into dev VM
and use `private-chef-ctl test`. If you can't do that for whatever
reason, you can run `bin/oc-chef-pedant -c CONFIG`. See
[multitenant_config.rb](multitenant_config.rb) for an example. Be sure
to correctly configure pedant for Ruby/Erlang as appropriate for your
test system.

When running oc-chef-pedant via `private-chef-ctl test`, pedant uses
configuration in `/var/opt/opscode/oc-chef-pedant/etc/pedant_config.rb`.
HTTP request data is logged to
`/var/log/opscode/oc-chef-pedant/http-traffic.log`.

When running tests manually, logging (and lots of other options) are
configured by command line switches. Run `bin/oc-chef-pedant -h` to list
them.

## Shared Functionality

Where functionality in OPC is shared/identical to that in Open Source
Chef Server, this test suite uses tests from
[chef-pedant](https://github.com/opscode/chef-pedant). By default, dev
VM is configured to mount your local chef-pedant checkout into the VM
when loading `oc-chef-pedant`. See the [dev VM config](https://github.com/opscode/opscode-dev-vm/blob/master/config/projects.json)
for details.


