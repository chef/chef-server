### Lua Tests

This directory contains tests related to the lua routing code. 
These tests that can be run in a non-functional server environment.
You should not need a running chef-server to perform these tests.
You simply have to set a few environment variables.

### If Running These Tests From A Functioning Chef Server

There Should be sane defaults for all the environment variables. Simply run:

```
./run_lua_tests.sh
```

### If running these tests locally from the opscode-omnibus repo

#### Install dependencies on your system

You will need a few dependencies. On mac:

```
brew install lua
luarocks install lpeg
luarocks install lua-cjson
```

On ubuntu:

```
sudo apt-get install lua5.1
sudo apt-get install luarocks
luarocks install lpeg
luarocks install lua-cjson
```

Also, the script itself will let you know if you are missing something.

#### Determine where lua is installed and set $LUALIB

The tests will need to symlink a few important files.
Set $LUALIB to the directory that contains `lpeg.so` and `cjson.so`, like so:

```
# if you don't know where those files live
$ find / -name lpeg.so

# use that result here
$ export LUALIB=<path to the files>

# for example
$ export LUALIB=/usr/local/lib/lua/5.2
```

#### Set $USE_OMNIBUS_FILES=0

```
$ export USE_OMNIBUS_FILES=0
```

This will let the script know that you are using local copies of the lua code in Chef template form.

#### Run the tests

```
./run_lua_tests.sh
```

The script will determine if you have the necessary components
installed, and will inform you as to next steps if not.  Once everything
is installed, it will run the tests.

