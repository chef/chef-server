
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

