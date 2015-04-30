Transitioning to Installer-based Vagrant
========================================

Earlier versions of `oc_bifrost` included a `Vagrantfile` geared
toward the old gem-based Vagrant executable.  This approach is now
**deprecated**; you should transition to using the new installer-based
Vagrant.  This documents the process and caveats involved in doing
this, with an eye toward getting `oc_bifrost` VMs running for local
development work.

## Download


Our `Vagrantfile` is geared for the latest installer-based versions.
If you do not already have this, please download the latest from
http://downloads.vagrantup.com.  Note that it __will not work__ with
earlier gem-based versions of Vagrant!

## Cleanup the Old

If you used earlier versions of this process with `bundle install
--binstubs`, go ahead and delete the `bin` directory now; you will not
need it anymore, and it will probably just confuse things and bring
you much woe and heartache.  Nobody needs that.

## A Foot in Both Worlds

If you still have old gem-based vagrant on your system, and you use
RBEnv, you may have some shims around.  Since `opscode-dev-vm` is
still using the old Vagrant, you probably don't want to remove your
vagrant gem and nuke your shim just yet (you could do a `bundle
install --binstubs` in `opscode-dev-vm` if you wanted to, but I
digress).  To ensure you're using the installer-based Vagrant, you can
invoke it directly using `/usr/bin/vagrant` and remove all doubt.

Due to incompatibilities between the on-disk representation of
Vagrant boxes for the gem-based and installer-based Vagrant, you may
run into issues if you need to continue to use both versions of
Vagrant.  The first time you run the installer-based Vagrant, it will
notify you that it will update the format of your existing boxes.  If
you accept this, these boxes will no longer work with the gem-based
Vagrant.  However, there is an environment variable (`VAGRANT_HOME`)
that you can set which will enable you to use different directories
for the different Vagrants.  If you update your boxes for
installer-based Vagrant (recommended), you'll need to set
`VAGRANT_HOME` to point to somewhere other than `~/.vagrant.d` when
using gem-based Vagrant in the future; if you don't update the boxes,
you'll need to set `VAGRANT_HOME` when running the installer-based
Vagrant.  The choice is yours.
