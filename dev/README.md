##  Welcome!

You have found the chef-server self-contained development environment.
It aims to get you up and hacking on chef-server components in a
safe, pain-free manner.

In short it will create a simple Vagrant VM, while still allowing you to do
your development from the comfort of your own host. Changes to make are
loaded immediately into the running service (for erlang projects with
sync support) or otherwise made available on the guest VM.

## Quick Start

This assumes familiarity with the components of chef-server and that you
want to run oc\_erchef off of your local machine.

Requirements:

* VirtualBox 4.3+
* Vagrant 1.7+
* At least one recent Chef Server 12.0.9+ debian package download,
  which you can get using this command (if you have the most recent chefdk) `mixlib-install download chef-server -p ubuntu -a x86_64 -l 14`. dvm will then look for the package in either the Downloads dir
  on your machine or the omnibus/pkg directory under the chef-server repo
  where dvm is running. You can also set the INSTALLER environment variable
  to tell dvm where to find the package if it is not in one of those locations.
* A text editor on your machine.
* Any time you're doing something with 'dvm' on the VM, make sure
  you've acquired root by using `sudo -i`. Otherwise dvm won't be in your
  path.

First, add the following configuration to your `/etc/hosts` file:

    192.168.56.100 api.chef-server.dev manage.chef-server.dev
    192.168.56.150 database.chef-server.dev
    192.168.56.151 backend.chef-server.dev
    192.168.56.152 ldap.chef-server.dev
    192.168.56.153 custom.chef-server.dev
    192.168.56.155 reportingdb.chef-server.dev
    192.168.56.156 elasticsearch.chef-server.dev

Next, bring up the VMs!

    cd dev
    vagrant up

Provisioning ensures that you are able to use a default organization and admin user
both in the VM as root, and from your host in the `dev` directory.

In a separate terminal session/pane/window:

    vagrant ssh
    sudo -i
    tmux # optional, allows you to disconnect and re-attach to a running sessino
    dvm load oc_erchef
    dvm start oc_erchef


### What can I do?

Start editing erchef files, pedant files, cookbooks, upgrade definitions,
and/or chef-server-ctl commands.

* Changes to erchef erlang files will be picked up and recompiled
  automatically shortly after you save them on the host.
* To test cookbook changes, load them with `dvm load omnibus server-ctl-cookbooks`.
  This will trigger a reconfigure. To avoid automatic reconfigure, set
  `projects.omnibus.components.reconfigure_on_load: false` in your config.yml.
  Any time you run `chef-server-ctl reconfigure` from this point forward it will
  reconfigure using the cookbooks on the host.
* To run pedant tests in the VM, use `dvm run oc-chef-pedant`.  You can also provide the
  usual flags, eg `dvm run oc-chef-pedant --focus-/skip-X`, `--smoke`, `--all`, etc.

`dvm` has support for many projects including bifrost and bookshelf.  Use `dvm list`
to see them all.

While all host changes are replicated to the dev vm only erlang projects support
automatic hot compile and reload of changed modules on the host.

### Installing Chef Server Plugins

If you wish to install Chef Server plugins with pre-downloaded or pre-built
binaries, set the corresponding attribute in your `config.yml` to true.
The corresponding package needs to be either in `~/Downloads`, `../omnibus/pkg`,
or you can set an environment variable with the path to the package.

| Plugin Name | config.yml Attribute | Environment Variable |
| ------ | -------------------- | -------------------- |
| Chef Management Console | `chef-manage` | `MANAGE_PKG` |
| Push Jobs Server | `push-jobs-server` | `PUSH_JOBS_PKG` |
| Chef Reporting | `reporting` | `REPORTING_PKG` |

For example, this `config.yml` file created in the current directory would enable the Management console.

```
vm:
  plugins:
    chef-manage: true
```

### Configuring LDAP

If you wish to setup your Chef Server to use an external LDAP service, please set
`ldap start` to true in your config.yml:

```
vm:
  ldap:
    start: true
```

This LDAP server comes pre-configured with two users: `child` and `douglas`. You can add more by adding more LDIF files
in the ldap-data directory of the provisioning cookbook. The password to authenticate matches the username.

Chef Server will be configured to perform user authentication using ldap, however users mapped to `child` and/or `douglas` must
be created on the server.

TODO: show user create command for creating a user with external ID

### Setting up Chef Backend

If you wish to setup a development environment that is a 1 frontend, 1 backend
HA topology, set `chef-backend start` to true in your config.yml. _Warning: Chef Backend
is not compatible with the Push Jobs Server, Chef Reporting or LDAP DVM configurations._

You can configure which package to install for chef-backend with the environment variable 'BACKEND_PKG'

### Installer Options

If you have a package at a non-default location (defaults are `../omnibus/pkg` and this directory),
you can specify it with the $SERVER_PKG variable. Also, you can automate the selection of which build
to use with $AUTOPACKAGE. So, to automatically select a custom package, set $INSTALLER to it's path
and $AUTOPACKAGE to 1.

### Multiple vms

If you are fortunate enough to have a host beefy enough to run
multiple VMs you will hit a snag; the 'chef-server' VM name is
global, and there can only be one. To use a different name, set the
environment variable export VAGRANT\_MACHINE\_VARIANT to add a suffix
to the vm names.

Example:
```
export VAGRANT_MACHINE_VARIANT='1'
```

### Coverage Analysis

Erlang has built-in support for code coverage analaysis which will
evaluate how often each line of code in erlang modules is hit.  To enable this
for a full project, use:

    dvm cover PROJECT enable

To enable it for a specific module, use:

    dvm cover PROJECT enable MODULE_NAME

To reset coverage stats for one or all modules:

    dvm cover PROJECT reset [MODULE_NAME]

To write coverage results to file (chef-server/testdata/coverage) use:

    dvm cover PROJECT report [MODULE_NAME]

And finally to disable coverage and re-enable automatic code sync:

    dvm cover PROJECT disable

Note that enabling cover will disable automatic compile and load of changed
modules, and disabling it will turn that back on.

## oc-id

When loading oc-id, certain files will be replaced.  This will be reflected on the host/git status - you should not
commit these modified files:

* `src/oc-id/config/database.yml`
* `src/oc-id/config/intializers/secret_token.rb`
* `src/oc-id/config/settins/production.yml`
* `src/oc-id/db/schema.rb`

The ruby environment does not support hot code loading.  In order to pick up changes made to oc-id
on the host, you will need to stop/start the service:

```
dvm stop oc-id; dvm start oc-id
```


## Erlang Project Dependencies

If you need to work on an erlang project dependency (for example, you want to modify chef_authn)
this can also be loaded into the VM with hot code loading support.  After loading the service
in the VM (eg. `dvm load oc_erchef`) , switch to the host and clone the dependency into `external-deps`
directory. For example, to load `chef_authn` into a loaded and running `oc_erchef`:

```
# From the host in chef-server/

cd external-deps
git clone https://github.com/chef/chef_authn
```

When you `dvm start oc_erchef`, or if you've already started it, you will see output when it picks
the replacement dependency code.

Note that other services running under dvm via `dvm start` will also load the `chef_authn` checkout.
Services running normally (under runit/chef-server-ctl) will not.

##  dvm

At the heart of all this is the 'dvm' tool.  This tool
is used to manage the environment from within
the vm.  It must always be run as root, and to be sure that the
environment is correct, use `sudo -i` to become root to run it.

The intention here was to create a simple way to do all the
common activities:

* load a project
* run a test suite or subset thereof
* run a project and/or start a loaded service
* open a console to a running erlang app
* access the DB for a project

And a few other things.  You can just use `dvm` by itself and a list of
commands will be generated. You may also want to take a look in
defaults.yml, in which project definitions and other things are set up.

# Using external databases

To use an external database for Chef Server (and Reporting), create a `config.yml` file with the following contents:

```
vm:
  postgresql:
    start: true
```

To use an external Azure database for Chef Server, create a `config.yml` file with the following contents:

```
vm:
  postgresql:
    start: true
    use-azure:    true
    ip-azure:     <AZURE POSTGRESQL IP ADDRESS>
```

To use separate external databases for Chef Server and Chef Reporting, create a `config.yml` file with the following contents:

```
vm:
  postgresql:
    start: true
  reporting_postgresql:
    start: true
```
# Using elasticsearch

To create a separate elasticsearch vm create a `config.yml` file with the following contents:
```
  elasticsearch:
    start: true
    # The major version family to use - either "2" or "5".
    version: "5"
```

