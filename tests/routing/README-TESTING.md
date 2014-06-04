
There are two sets of tests associated with the API load balancers.

### Lua Tests

The first is a set of unit tests that are written in Lua.
These tests validate the behavior of each routing component: URL
resolution, upstream resolution, route access and logic checks.

These may be executed without starting the internal and external LB vms:

```
   cd tests
   ./run_lua_tests.sh
```

The script will determine if you have the necessary components
installed, and will inform you as to next steps if not.  Once everything
is installed, it will run the tests.

### Spec Tests

A suite of rspec tests is also available. These tests send requests to
the local vagrant instances to validate the end-to-end routing behavior across
urls and redis configurations.  Each request runs through to sending it
to its upstream (if appropriate) and validating the correct upstream
received the request.

After performing "Initial Vagrant Instance Setup" below, run the tests
as follows:

```
   cd tests
   bundle install # just need to do this the first time
   bundle exec rspec
```

#### Initial Vagrant Instance Setup

Prior to running the spec tests you'll need to ensure that both the internal and
external vagrant LB instances are up

```
    vagrant up opscode-lb-external
    vagrant up opscode-lb-internal
```

Both of these will fail due to no search response.  This is expected,
and occurs due to the loading order of the AWS gem.  To complete
bringing the VMs up:

```
    vagrant provision opscode-lb-external
    vagrant provision opscode-lb-internal
```

Once both VMs are configured, you will need to take a couple of
additional steps to get them ready for use. Noet that

First external LB - this is required only after initial nginx verison is
installed on the 'provision' step above. This is because the
build and installation from source of nginx causes it to be deployed
running withou tfirst having our configuration loaded. (Future updates
will correct this.)

```
    vagrant ssh opscode-lb-external
    sudo service nginx stop
    sudo killall -9 nginx
    sudo service nginx start
    exit
````

Now the internal LB. This is configured as part of an HA pair, so by
default is not auto-started.

```
   vagrant ssh opscode-lb-internal
   sudo service nginx start
   exit
```



