Setup Mac OS X for Running Chef Server pgTAP Tests
==================================================

For running on Mac OS X, [Postgres.app][] is hands-down the easiest.
Download and install!  Turn it on and you've got a database server!

You'll want to add the PostgreSQL binaries to your path before you do
anything else, though.

```
export PATH="/Applications/Postgres.app/Contents/MacOS/bin:$PATH"
```

Installing pgTAP into this server is straightforward as well.

```
git clone git://github.com/theory/pgtap.git
cd pgtap
make
make installcheck
make install
```

To run the pgTAP tests, you will need to install [pg_prove][]:

```
sudo cpan TAP::Parser::SourceHandler::pgTAP
```

Additionally, we have created [chef_pgtap][], an extension on top of
pgTAP that provides some more testing functions.  Some of these are
general-use and (with a little work) could make it into pgTAP itself,
while others are specific to testing the Chef schema.

```
git clone git://github.com/opscode/chef_pgtap.git
cd chef_pgtap
make install
```

You should be good to go now!

[pg_prove]:http://pgtap.org/pg_prove.html
[Postgres.app]:http://postgresapp.com
[chef_pgtap]:http://github.com/opscode/chef_pgtap
