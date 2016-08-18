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
  which you can grab from https://bintray.com/chef/current/chef-server. Note
  that you should download the chef-server-core package, and not run the
  installer. dvm will then look for the package in either the Downloads dir
  on your machine or the omnibus/pkg directory under the chef-server repo
  where dvm is running. You can also set the INSTALLER environment variable
  to tell dvm where to find the package if it is not in one of those locations.
* A text editor on your machine.
* Any time you're doing something with 'dvm' on the VM, make sure
  you've acquired root by using `sudo -i`. Otherwise dvm won't be in your
  path.

First, add the following configuration to your `/etc/hosts` file:

    192.168.33.100 api.chef-server.dev
    192.168.33.150 database.chef-server.dev
    192.168.33.151 backend.chef-server.dev
    192.168.33.155 reportingdb.chef-server.dev

Next, bring up the VMs!

    cd dev
    vagrant up
    ./sync

In a separate terminal session/pane/window:

    vagrant ssh
    sudo -i
    dvm quickstart oc_erchef

### What can I do?

Start editing erchef files, pedant files, cookbooks, upgrade definitions,
and/or chef-server-ctl commands.

* Changes to erchef erlang files will be picked up and recompiled
  automatically shortly after you save them on the host.
* To test cookbook changes, load them with `dvm load omnibus private-chef-cookbooks`.
  Then run `chef-server-ctl reconfigure` in the VM to pick up the changes.
* upgrades and chef-server-ctl command changes/additions will be
  available very quickly after you save them on the host (< 5 seconds)
* To run pedant tests in the VM, use `dvm run oc-chef-pedant`.  You can also provide the
  usual flags, eg `dvm run oc-chef-pedant --focus-/skip-X`, `--smoke`, `--all`, etc.

`dvm` has support for many projects including bifrost and bookshelf.  Use `dvm list`
to see them all.

While all host changes are replicated to the dev vm only erlang projects support
automatic hot compile and reload of changed modules on the host. To whatever
extent supported by the language, we'll be adding the same for ruby service-based projects.

### Dependency Loading

If you find that you need to change an erlang project
dependency, dvm simplifies that too. For example, let's say we want to
modify `chef_authn` and pull it into the running erchef instance:

    # From in the vm. assumes sudo -i
    dvm load oc_erchef chef_authn

This will  clone `chef_authn` onto your host[1], where you can begin
editing it. It will link it into the project deps directory and hot-load
it into the running VM[2].  This is available for nearly all dependencies
declared in a project's rebar.config.

[1] NOTE: Presently this will clone into chef-server directory. We will be
 fixing this, it's a side effect of the recent project merge.
[2] There is currently a limitation here in that the owning project must
be running to pick up the changes. We will be fixing that shortly.

For a list of dependencies available for loading and their current
status, use `dvm list $PROJECTNAME`.  Any dependency that is not a
system library and declared in a project's app.src is typically
available.

Ruby project dependency loading support coming soon.

### Installer Options

If you have a package at a non-default location (defaults are `../omnibus/pkg` and this directory),
you can specify it with the $INSTALLER variable. Also, you can automate the selection of which build
to use with $AUTOPACKAGE. So, to automatically select a custom package, set $INSTALLER to it's path
and $AUTOPACKAGE to 1.

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

## Troubleshooting

If you see something like the following, or ever have trouble with Vagrant while running `./sync`:

```
Files Transferred: 0
         Next Sync: 0 seconds...Bad port '-o'force_available_locales will default to true in the future. If you really want to skip validation of your locale you can set I18n.enforce_available_locales =rsync: connection unexpectedly closed (0 bytes received so far) [sender]
rsync error: error in rsync protocol data stream (code 12) at /SourceCache/rsync/rsync-45/rsync/io.c(453) [sender=2.6.9]

It appears that you've ran a newer version of Vagrant on this
computer. Unfortunately, newer versions of Vagrant change internal
directory layouts that cause older versions to break. This version
of Vagrant cannot properly run.

If you'd like to start from a clean state, please remove the
Vagrant state directory: /Users/tyler/.vagrant.d

Warning that this will remove all your boxes and potentially corrupt
existing Vagrant environments that were running based on the future
version.
```

It might be because ruby is picking up the ChefDK version of Vagrant instead of the one you normally use. For example:

```
which vagrant
/usr/local/bin/vagrant
++> irb
`irb(main):001:0> `which vagrant`
=> "/opt/chefdk/embedded/bin/vagrant\n"
```

You can tell `./sync` what Vagrant to use by setting `$VAGRANT_PATH` like so:

```
export VAGRANT_PATH=/usr/local/bin/vagrant
```

## Chef Mover

`chef-mover` does not currently support hot code loading. You can use it, but you have
to interact with it a bit differently. To start it, run:

```
dvm load chef-mover
dvm start chef-mover
```

This will start it up, but if you make any changes, you will need to reload the application, like:

```
dvm load chef-mover --force
dvm start chef-mover
```

Then the new code will be loaded.

## Not So Quick Start

TODO: details of components, terminology, etc. explore more dvm
commands?

## Test Data

[pending] If you want to load in test users, orgs, and nodes for testing outside
of pedant, you can:

`dvm populate`

TODO: Provide a canned set of test data in ec-backup form for more thorough tests?
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
* etop a running erlang app
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
    use-external: true
```

To use separate external databases for Chef Server and Chef Reporting, create a `config.yml` file with the following contents:

```
vm:
  postgresql:
    start: true
    use-external: true
  reporting_postgresql:
    start: true
    use-external: true
```
