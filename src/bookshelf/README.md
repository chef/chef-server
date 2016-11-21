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

Run the integration tests with rebar:

    $> ./rebar3 ct

By default the integration tests start a server to test. Testing the
postgres store requires an existing postgres instance on the same
host as the tests.

This is provided when running in the development environment; but
either the user running has to have access to the db, or the password
and user set via environment variables:

    $> export CT_SQL_PASSWORD=b7e8d6c98e03c3a981c0d0464f13335893ebcdd4e643bd603f7bb77e8236846101a4fdd7db13c8cd
    $> export CT_SQL_USER=bookshelf

The user & password can be found in the bookshelf config (/var/opt/opscode/bookshelf/sys.config)

The tests can also be run against an existing server by setting
STANDALONE\_BOOKSHELF flag in bkswt\_api\_SUITE.erl to
'true'. However the tests rely on being able to flip between sql and
filesystem modes, and some tests will not work correctly because of
this; most likely you will want to comment out the filesystem or sql
tests before running.

##### All Tests

    $> rebar3 do eunit, ct



##### Stress tests
See the README in the stest directory

Configuration
-------------

  Standard OTP application configuration

Start
-----

    $>  ./_build/default/rel/bookshelf/bin/bookshelf console


Notes on chef-server/dev environment
---------------------
The easiest way to work on bookshelf improvements is to use the
chef-server/dev env

Start up dev environment and sync process

Start up a shell for the bookshelf console
  Login to the vm via 'vagrant login'
  dvm load bookshelf
  dvm start bookshelf

Start up a second shell for tests
cd /host/src/bookshelf
./rebar3 ct

It's often worth tunneling 4321 (the bookshelf port) through to the
host box; the easiest way is to:
    $> cd chef-server/dev
    $> ssh vagrant@127.0.0.1 -p 2222 -o Compression=yes -o DSAAuthentication=yes -o LogLevel=FATAL -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -o IdentitiesOnly=yes -i .vagrant/machines/chef-server/virtualbox/private_key -o ForwardAgent=yes -L4321:localhost:4321

The port (2222 here) may change depending on what other VMs are running.


#### Database work

To gain access to the database, one can log in to the dev VM and run
    $> sudo su opscode-pgsql -c "/opt/opscode/embedded/bin/psql bookshelf"

Running sqitch requires a database password:
    $> export PGPASSWORD=<INSERT_SQL_PASSWORD>
    $> sqitch --db-name bookshelf -u opscode_chef --db-host localhost deploy



#### S3 API

s3cmd can be useful for testing the API. Set up a .s3cfg with the
defaults except:
secret\_key = <SECRET\_KEY>
access\_key = <ACCESS\_KEY>
proxy\_host = localhost
proxy\_port = 4321

It can be useful to access the API with low level HTTP commands. If
you don't want to go to the trouble of generating correct signatures,
setting the config variable bookshelf:auth\_check\_disabled will cause
bookshelf to ignore bad signatures.

Useful commands include
    $> s3cmd -d --config .s3cfg ls  # list buckets:
    $> s3cmd -d -v --config .s3cfg mb boing # create bucket
  
Then curl/wget will work to get/put files: 
    $> curl -i -X PUT  localhost:4321/foo/README.md3  -d@README.md


