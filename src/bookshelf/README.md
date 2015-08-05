Bookshelf
=========

Description
-----------

  Bookshelf is an S3 API compatible object store.

Setup
-----

*[Erlang](http://www.erlang.org) 17.5
*[Rebar](https://github.com/rebar/rebar3) (3+)
*[Virtualenv](http://pypi.python.org/pypi/virtualenv) (for integration testing)

### Host Name Setup

s3 works by specifying the bucket name as part of the domain name
being resolved. So, for example, if we where trying to create bucket
`foo` we would post a `PUT` to the domain `foo.localhost.localdomain`
(in the common case). Unfortunately, that is not going to work on your
local box. You can not specify wild cards for something like
*.localhost.localdomain. To get around this problem we need to use
some other tool. The recommended tool fir this is
[dnsmasq](http://www.thekelleys.org.uk/dnsmasq/doc.html). Get dnsmasq
installed and then add

    address=/.localhost.localdomain/127.0.0.1

to `/etc/dnsmasq.conf`. This should make all buckets resolvable. It has some consequences for


Build
-----

    $> make

Test
----

Just as a note, the password for the testing cert is `opscode`.

##### Unit Tests

    $> rebar3 eunit

##### Integration Tests

The integration tests do *not* start the server to be tested. You must
manually run $>./start.sh to start that instance before runing `make test`

    $> rebar3 ct

##### All Tests

    $> rebar3 do eunit, ct


Configuration
-------------

  Standard OTP application configuration

Start
-----

    $>  ./_build/default/rel/bookshelf/bin/bookshelf console
