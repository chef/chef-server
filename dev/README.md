##  Welcome!

You have found the chef-server self-contained development environment.
It aims to get you up and hacking on chef-server components in a
safe, pain-free manner.

In short it will create a simple Vagrant VM, while still allowing you to do
your development from the comfort of your own host. Changes to make are
either instantly loaded (erlang projects with sync support) or instantly
available on the guest vm.

## Quick Start

Assuming you'd like to be able to modify oc_erchef, its omnibus
cookbooks, and run/modify pedant tests:

```
cd dev
vagrant up  # this takes 3-5 minutes depending on your machine
vagrant rsync-auto > /dev/null 2>&1 & 
vagrant ssh
sudo -i
tmux # recommended, not required.
dvm quickstart oc_erchef
```

And if you want to verify that things are behaving ok, from inside the
VM as root:
```
dvm run oc-chef-pedant
```

You can also provide the usual flags, eg `dvm run oc-chef-pedant
--focus-/skip-X`, `--all`, etc.

Now, start modifying things on your host. You'll notice the
following:

* Any change to erchef files will automatically compile and (if no
  errors) load into the running erchef instance now on your screen.
  You can see this activity happen in the running instance.
* changes to opscode-omnibus/fils/private-chef-cookbooks are sync'd on the guest.
  Change them on the host then run reconfigure to pick up the changes.
* Similarly oc-chef-pedant is sync'd.  At any time you want to run  your
  modifications you can `dvm run oc-chef-pedant`. Focus tags, etc are
  respected normally.

If you want to load in omnibus partybus upgrade migrations or commands
for testing you can (from within the vm):

* dvm load omnibus upgrades
* dvm load omnibus ctl-commands

Note that the standard basic VM management comments for vagrant
(suspend/halt/destroy/etc) work normally.

TODO: If you want to load in test users, orgs, and nodes for testing outside
of pedant, you can:

`dvm populate all`

Or if you don't want anything:
* `dvm populate users
* `dvm populate orgs` (Also creates users listed as associated with the orgs.)
* `dvm populate nodes

### Wait what just happened?

The dvm `quickstart` command loads several projects for you. Take a
look at defaults.yml under "quickstart" and you will see:

```
    load:
      - omnibus private-chef-cookbooks
      - oc-chef-pedant
      - oc_erchef
    start:
      - oc_erchef --foreground
```

What this is the equivalent of doing is:

```
dvm load omnibus private-chef-cookbooks # load in cookbooks
dvm load oc-chef-pedant # load in oc-chef-pedant  bundle installing as needed
dvm load oc_erchef # build and load oc_erchef
dvm start oc_erchef  --foreground
```

And it does just what it says. You can add your own quickstart
configurations - or override the existing ones - in config.yml.
Currently 'load' and 'start' are the supported sections since in
practice that's what's been neeeded so far.


## PreReqs

Assuming that you're looking to do oc_erchef and/or opscode-omnibus
development:

* 8vcores and enough ram to do development with (16GB+, but 8 might
  work...)
* vagrant 1.7+ - earlier versions may work but are not tested
* virtualbox 4.3 (as tested, other versions may/may not work)
* A directory containing your checked out chef server projects. These
  should be at the same level. Because this directory is rsync'd with the
  VM (other options didn't give acceptable performance) try to avoid including
  too many unrelated projects. For the erchef quickstart you will need:
  * oc_erchef
  * opscode-omnibus
  * oc-chef-pedant

###  dvm

dvm is a command line tool used to manage the environment from within
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

