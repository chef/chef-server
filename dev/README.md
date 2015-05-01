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
want to

Requirements:

* VirtualBox 4.3+
* Vagrant 1.7+
* At least one recent Chef Server 12.0.9+ debian package download,
  which you can grab from https://packagecloud.io/chef/current. Note
  that you should download the chef-server-core package, and not run the
  installer.
* A text editor on your machine.
* Any time you're doing something with 'dvm' on the VM, make sure
  you've acquired root by using `sudo -i`. Otherwise dvm won't be in your
  path.

Go!

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
* To pick up cookbook changes, run `chef-server-ctl reconfigure` in the VM.
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

## Not so quick start

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
* coming soon: enable coverage for an erlang project

And a few other things.  You can just use `dvm` by itself and a list of
commands will be generated. You may also want to take a look in
defaults.yml, in which project definitions and other things are set up.

