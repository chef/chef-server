oc_authz
========

Testing and Development
=======================

We're using [Berkshelf][] to make things easy.

First, you'll need to configure Berkshelf to talk to our preprod Chef
Server.  Create or add to a `~/.berkshelf/config.json` file the
following information:

``` javascript

{
    "chef": {
        "chef_server_url": "https://opsmaster-api.opscode.us/organizations/preprod",
        "node_name": <YOUR_OPSMASTER_ACCOUNT_NAME>,
        "client_key": <PATH_TO_YOUR_OPSMASTER_SSH_KEY>
    }
}
```

Then, grab all the dependencies.  We're installing binary stubs into
`bin` to ensure everything is as self-contained as possible.

```
bundle install --binstubs
```

Now, to fire up and provision a VM:

```
bin/vagrant up
```

Go muck around on the machine now:

```
bin/vagrant ssh
```

To re-run `chef-client` on your test machine:

```
bin/vagrant provision
```

If you screw something up horribly, just destroy the machine and start
again:

```
bin/vagrant destroy
```

Eventually, we'll be adding Test Kitchen support for running
[oc-authz-pedant][] and our pgTAP database schema tests.

Cookbook Hacking
================

If you want to hack on the [opscode-authz][] cookbook, you'll need to
make a minor tweak to the Berksfile.

First, perform a local checkout of the cookbook.  Then, modify the
dependency line in `Berksfile`.

Change this:

``` ruby
cookbook "opscode-authz", git: "git@github.com:opscode-cookbooks/opscode-authz"
```

to this:

``` ruby
cookbook "opscode-authz", path: "/path/to/local/checkout/of/opscode-authz"
```

Re-provision your machine and you'll be running off the local version
of the cookbook.


[Berkshelf]:http://berkshelf.com
[oc-authz-pedant]:https://github.com/opscode/oc-authz-pedant
[opscode-authz]:https://github.com/opscode-cookbooks/opscode-authz
