Chef Server Omnibus Project
============================

[![Build Status Master](https://travis-ci.org/chef/opscode-omnibus.svg?branch=master)](https://travis-ci.org/chef/opscode-omnibus)


This project creates full-stack platform-specific packages for
`chef-server`!

## License

All files in the repository are licensed under the Apache 2.0 license. If any
file is missing the License header it should assume the following is attached;

```
Copyright 2014 Chef Software Inc

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
```

Contribution
------------

Please follow the [contribution guidelines](CONTRIBUTING.md) when submitting pull requests. Specifically, any significant change should be accompanied by an update to either the changelog or release notes. PRs should not be merged without either such an update or verification that this change is small enough to not need to update the changelog or release notes.

Installation
------------
You must have a sane Ruby 1.9+ environment with Bundler installed. Ensure all
the required gems are installed:

```shell
$ bundle install --binstubs
```

Testing
-------
To run the unit tests:

```
bundle install --binstubs
./bin/rake test:routing
```

Usage
-----
### Build

You'll need to **create an omnibus.rb** file based on the
omnibus.rb.example.rb.  Please grab credentials from teampass.


You create a platform-specific package using the `build project` command:

```shell
$ bin/omnibus build chef-server
```

The platform/architecture type of the package created will match the platform
where the `build project` command is invoked. For example, running this command
on a MacBook Pro will generate a Mac OS X package. After the build completes
packages will be available in the `pkg/` folder.

### Clean

You can clean up all temporary files generated during the build process with
the `clean` command:

```shell
$ bin/omnibus clean chef-server
```

Adding the `--purge` purge option removes __ALL__ files generated during the
build including the project install directory (`/opt/opscode`) and
the package cache directory (`/var/cache/omnibus/pkg`):

```shell
$ bin/omnibus clean chef-server --purge
```

### Help

Full help for the Omnibus command line interface can be accessed with the
`help` command:

```shell
$ bin/omnibus help
```

Makefile instructions
---------------------

You can change the dev platform (anywhere it says "ubuntu 1004" below)
to one of your choosing by setting the environment variable
`DEV_PLATFORM`

**Note:** Any `make` or `kitchen` commands expect `opscode\omnibus`
  and `opscode\omnibus-software` to be cloned from github in `..`

#### Top level make targets:

`$ make dev` converges the dev platform (ubuntu 1004)

`$ make dev-login` logs into the dev platform (ubuntu 1004)

`$ make dev-build` will execute an omnibus build in the dev platform (ubuntu 1004)

`$ make dev-destroy` will destroy the current dev platform (ubuntu 1004)

`$ make dev-suspend` will suspend the current dev platform (ubuntu 1004)

`$ make dev-resume` will resume the current dev platform (ubuntu 1004)

`$ make update` will update omnibus and omnibus-software

`$ make extract_dev_cache` will copy the git cache to the current directory

`$ make deploy_dev_cache` will copy a local git cache into the current build environment

There is some additional tooling in the makefile to cleanup stale
builds for long running environments.  This will prevent builds from
eating disk space.


The usual practice would be something like

`$ make update dev dev-build`

That would update omnibus, omnibus-software, converge a build
environment and then build the package.

Kitchen-based Build Environment
-------------------------------
Every Omnibus project ships will a project-specific
[Berksfile](http://berkshelf.com/) that will allow you to build your omnibus projects on all of the projects listed
in the `.kitchen.yml`. You can add/remove additional platforms as needed by
changing the list found in the `.kitchen.yml` `platforms` YAML stanza.

This build environment is designed to get you up-and-running quickly. However,
there is nothing that restricts you to building on other platforms. Simply use
the [omnibus cookbook](https://github.com/chef-cookbooks/omnibus) to setup
your desired platform and execute the build steps listed above.

The default build environment requires Test Kitchen and VirtualBox for local
development. Test Kitchen also exposes the ability to provision instances using
various cloud providers like AWS, DigitalOcean, or OpenStack. For more
information, please see the [Test Kitchen documentation](http://kitchen.ci).

Once you have tweaked your `.kitchen.yml` (or `.kitchen.local.yml`) to your
liking, you can bring up an individual build environment using the `kitchen`
command.

**NOTE:** Test Kitchen shoud be installed external to the local Ruby bundle.
Please either use ChefDK or install the latest test-kitchen from Rubygems.

```shell
$ kitchen converge ubuntu-1604
```

Test Kitchen uses a regex syntax to match on plaforms, so for example ubuntu 10.04
will be specificed as ubuntu-1004, or even just ubuntu-10, if 10.04 is the
only 10 series specified in the `.kitchen.yml`.

Then login to the instance and build the project as described in the Usage
section:

```shell
$ kitchen login ubuntu-1604
[vagrant@ubuntu...] $ . load-omnibus-toolchain.sh
[vagrant@ubuntu...] $ cd chef-server/omnibus
[vagrant@ubuntu...] $ sudo chown -R vagrant ~/.bundle/ 
[vagrant@ubuntu...] $ bundle install --binstubs
...
[vagrant@ubuntu...] $ bin/omnibus build chef-server -l internal
```
or if you prefer not to use binstubs and to use bundle exec instead:

```shell
$ kitchen login ubuntu-1604
[vagrant@ubuntu...] $ . load-omnibus-toolchain.sh
[vagrant@ubuntu...] $ cd chef-server/omnibus
[vagrant@ubuntu...] $ sudo chown -R vagrant ~/.bundle/
[vagrant@ubuntu...] $ bundle install
...
[vagrant@ubuntu...] $ bundle exec omnibus build chef-server -l internal
```

For a complete list of all commands and platforms, run `kitchen list` or
`kitchen help`.
