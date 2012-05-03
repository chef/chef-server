Bookshelf
=========

Description
-----------

  Bookshelf is an S3 API compatible object store.

Setup
-----

*[Erlang](http://www.erlang.org) R15
*[Rebar](https://github.com/basho/rebar) (recent)
*[Virtualenv](http://pypi.python.org/pypi/virtualenv) (for integration testing)

You should also make sure that `localhost.localdomain` resolves to
your local ip address (usually 127.0.0.1). This is currently *not* the
case (at least on ubuntu).

The integration tests do *not* start the server to be tested. You must
manually run $>./start.sh to start that instance before runing `make test`

Build
-----

    $> make

Test
----

    $> make test

Configuration
-------------

  Standard OTP application configuration

Start
-----

    $> ./start.sh
