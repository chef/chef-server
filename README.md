oc-authz-pedant
===============

Testing the [oc_heimdall][] REST API

# Testing against oc_heimdall running in a VM

Fire up a VM for `oc_heimdall`:

```
cd ${PLACE_WHERE_YOU_KEEP_CODE}/oc_heimdall
bin/vagrant up
```

(Please consult [oc_heimdall's README][] for more details on setting
up your Berkshelf / Vagrant environment.)

Now, run Pedant against that VM (from within this directory).

```
bin/oc-authz-pedant --config vm_pedant_config.rb
```

The `vm_pedant_config.rb` file is set up with the same port that
`oc_heimdall`'s [Vagrantfile][] uses.

[oc_heimdall]:https://github.com/opscode/oc_heimdall
[oc_heimdall's README]:https://github.com/opscode/oc_heimdall/blob/master/README.md
[Vagrantfile]:https://github.com/opscode/oc_heimdall/blob/master/Vagrantfile
