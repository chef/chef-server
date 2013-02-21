oc_heimdall
========

Heimdall?
=========

`oc_heimdall` is the Opscode Authorization API server.  It is named
after [Heimdall][], the all-seeing, all-hearing watchman of the
rainbow bridge to Asgard in Norse mythology.

`oc_heimdall` is a complete rewrite and replacement of the old
[opscode-authz][] API server.

[Heimdall]:http://en.wikipedia.org/wiki/Heimdallr
[opscode-authz]:https://github.com/opscode/opscode-authz

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

Also, while we still have a monolithic [chef repo][], we'll need to
refer to our platform roles and data bags in order to replicate our
production environment as much as possible in a local Vagrant setting.
The easiest way to make this work is to set an environment variable
that points to a local checkout of the platform cookbooks repo, which
the [Vagrantfile](Vagrantfile) then uses.

```
export OPSCODE_PLATFORM_REPO=/path/to/local/checkout/of/repo
```

Now you're ready to grab all the dependencies.  We're installing
binary stubs into `bin` to ensure everything is as self-contained as
possible.

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
[oc-heimdall-pedant][] and our pgTAP database schema tests.

Cookbook Hacking
================

If you want to hack on the [opscode-heimdall][] cookbook, you'll need to
make a minor tweak to the Berksfile.

First, perform a local checkout of the cookbook.  Then, modify the
dependency line in `Berksfile`.

Change this:

``` ruby
cookbook "opscode-heimdall, git: "git@github.com:opscode-cookbooks/opscode-heimdall"
```

to this:

``` ruby
cookbook "opscode-heimdall", path: "/path/to/local/checkout/of/opscode-heimdall"
```

Re-provision your machine and you'll be running off the local version
of the cookbook.

[Berkshelf]:http://berkshelf.com
[oc-heimdall-pedant]:https://github.com/opscode/oc-heimdall-pedant
[opscode-heimdall]:https://github.com/opscode-cookbooks/opscode-heimdall
[chef repo]:https://github.com/opscode/opscode-platform-cookbooks
