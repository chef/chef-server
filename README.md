oc-bifrost-pedant
===============

Testing the [oc_bifrost][] REST API

# Testing against oc_bifrost running in a VM

Fire up a VM for `oc_bifrost`:

```
cd ${PLACE_WHERE_YOU_KEEP_CODE}/oc_bifrost
bin/vagrant up
```

(Please consult [oc_bifrost's README][] for more details on setting
up your Berkshelf / Vagrant environment.)

Now, run Pedant against that VM (from within this directory).

```
bin/oc-bifrost-pedant --config vm_pedant_config.rb
```

The `vm_pedant_config.rb` file is set up with the same port that
`oc_bifrost`'s [Vagrantfile][] uses.

[oc_bifrost]:https://github.com/opscode/oc_bifrost
[oc_bifrost's README]:https://github.com/opscode/oc_bifrost/blob/master/README.md
[Vagrantfile]:https://github.com/opscode/oc_bifrost/blob/master/Vagrantfile
