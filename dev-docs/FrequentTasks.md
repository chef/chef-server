This is intended to document frequently executed tasks and provide
checklists of TODO's to make sure they are straightforward.

Do *NOT* accept the commit level BuildKite checks as
sufficient. Always do a complete BuildKite build and run of chef
pedant before merge.

## Getting Started
 - Generally all the updates are done inside the dev-vm /host/
   to avoid inconsistensies with ruby versions.
 - stop the sync function if it is running

## Updating Ruby
- omnibus_overrides.rb needs to be updated with the latest acceptable
  version of Ruby.

- Expeditor and BuildKite scripts
  It is best to keep the version we're using for tests the same as
  what we're shipping in Chef Server (match with omnibus_overrides)
    - scripts/bk\_tests/bk\_install.sh
    - .expeditor/verify.pipeline.yml
    - .expeditor/license_scout.sh

- Currently the Habitat plan.sh files use core/ruby; while as of this
  writing (2019-04-04) that is 2.5.5 that floats independently. Be
  aware that this can change, and perhaps consider using core/rubyXX
  instead.

## Updating Ruby Gems via Dependabot (automated)

Dependabot creates PRs for updating Ruby gems.  Verify pipeline builds
are kicked-off automatically, but it is necessary to verify that they
pass.  Any needed adhoc builds must be kicked-off by hand.

Some Dependabot PRs will only need passing verify tests.  Others will
need passing verify tests, adhoc builds, and umbrella tests.  Look at the title of the PR,
then consult the lists below to determine what you need to do with the PR.

Needing passing verify tests only:
- src/oc-id/Gemfile.lock
- src/oc\_erchef/apps/chef_objects/priv/depselector\_rb/Gemfile.lock
- src/oc_bifrost/oc-bifrost-pedant/Gemfile.lock

Needing passing verify + adhoc + umbrella:
- omnibus/Gemfile.lock
- oc-chef-pedant/Gemfile.lock
- src/chef-server-ctl/Gemfile.lock

You can rebase the PR's branch onto main by putting:
`@dependabot rebase`
in the comment section.

## Updating Ruby Gems by Hand

### Overview

To avoid frustration, remove all .bundle and vendor directories before
starting and after you complete the updates.

The following Gemfile.locks need to be updated to do a complete
sweep. We strongly recommend using bundler 1.17.x until bundler 2.1 is
out and stable.

- omnibus/Gemfile.lock
- oc-chef-pedant/Gemfile.lock
- src/oc-id/Gemfile.lock
  DO NOT update this file unless Rails is upgraded.
  Due to the Rails version, we've locked a lot of dependencies. Make
  sure we can build and run oc-id before merging to main.
  You'll need the libsqlite3-dev library if doing this on Ubuntu.
- src/oc\_erchef/apps/chef_objects/priv/depselector\_rb/Gemfile.lock
- src/chef-server-ctl/Gemfile.lock
- src/oc_bifrost/oc-bifrost-pedant/Gemfile.lock

Note:
- omnibus/Gemfile does *not* have a committed Gemfile.lock. We float on
  omnibus-software deliberately to keep up with the latest build
  dependencies.

Note:
- It's best to run
    - bundle install --binstubs --path=vendor/bundle
    - bundle update
  in the appropriate directories to keep your main ruby install
  clean. However do not forget to clean those up before doing an
  omnibus build, as the files there (especially .so files) can cause
  strange build failures, especially during the final consistency checks.

Note:
- On the dev-vm /dotfiles dir is mounted from your host. It is useful for
copying files back from the dev-vm to the host.
- The ./sync utility is used to copy files from the host into dev-vm.

### For Erlangers

Gems are like rebar dependencies, Gemfiles are like rebar.config files,
and Gemfile.lock files are like rebar.lock files. They are located in
directories for particular services the same way rebar config files are.

### Basic Steps to Update a Gem

1) `cd` to the directory containing the Gemfile which needs updating.

2) Rename or otherwise backup and delete the Gemfile.lock.

3) Edit the Gemfile and/or the .gemspec file to update the gem(s).

4) If gems in other directories need updating, go back to step 1,
otherwise proceed.

5) `ssh` into a dev vm.  Do not `sudo -i` or otherwise login as root.

6) `cd` to the directory containing the edited Gemfile and/or .gemspec
file, using /host as the root directory, e.g. /host/src/chef-server-ctl.

7) Remove all .bundle and vendor directories before starting and after you
complete the update:
```
rm -rf vendor
sudo rm -rf .bundle
```
8) Copy over your edited Gemfile and/or .gemspec file(s) (from step 3),
or otherwise make the same edits. chef-server/dev/dotfiles can be used
from your host to copy over files, and /dotfiles on your vm.

9) Run the following to generate a new Gemfile.lock:
```
bundle install --binstubs --path=vendor/bundle
bundle update
```

10) Copy the newly-generated Gemfile.lock back to the appropriate host
directory.

11) If other gems need updating, go back to step 6, otherwise you are
done.

### Steps to Update the PG Gem in chef-server

1) `cd` to the directory containing the .gemspec in chef-server-ctl for PG gem update.

2) Rename or otherwise backup and delete the Gemfile.lock.

3) Edit the .gemspec file to update the PG gem(s).

4) chef-server-ctl depends on PG gem and chef_fixie which also depends on the PG gem.

5) Make sure to update chef_fixie gem in the rubygem.org with same PG gem version otherwise chef-server-ctl fails with dependency conflict error.

5) After publishing the `chef_fixie`, The chef-server gemspec changes can be tested in dev vm

6) `ssh` into a dev vm.  Do `sudo -i` or otherwise login as root.

6) `cd` to the directory containing the edited .gemspec
file, using /host as the root directory, e.g. /host/src/chef-server-ctl.

7) generate .gem file
```
/opt/opscode/embedded/bin/gem build *.gemspec
```
8) install .gem file
```
/opt/opscode/embedded/bin/gem install *.gem
```

## Updating Chef Client

- Ensure that the versions in the following locations are consistent.
    - omnibus_overrides.rb
      You will also need to update Ohai
    - src/chef-server-ctl/chef-server-ctl.gemspec
      Don't forget to update the src/chef-server-ctl/Gemfile.lock

    - src/oc-id/ [leave alone for now]
      chef-client is neededed to provide the Chef Server API.
      We are currently using an old version of chef due to tech debt
      around rails upgrades. We should either fix that debt or find
      another library for the Chef Server API.

    - src/chef-server-ctl/Gemfile.local

- Build Chef Server, and do a chef-server-ctl reconfigure. Fix any new
  warnings about deprecations and the like.

## Updating rebar3

- It is helpful to periodically update the copy of the rebar3 tooling
  that is included with Chef Infra Server.
- The following erlang apps need to be updated:
  - bookshelf
  - oc_erchef
  - oc_bifrost
- Check for the current rebar3 version with `./rebar3 --version`
- Latest version for rebar3 can be found at: https://www.rebar3.org/
- Download and update the current rebar3 executable with it.

## Updating Erlang Dependencies using rebar3

- There are 2 approaches to updating the dependencies
  - From the dev-vm (Preferred method)
  - From the host machine (mac in most cases)
- Updating the erlang deps from the dev-vm
  - cd chef-server/dev
  - vagrant up
  - vagrant ssh
  - In another window/tab on the host machine
    - make changes to the rebar.config if needed
      and run sync to sync the changes with the dev-vm.
    - Make sure to stop the sync after it is complete.
  - In the dev-vm
    - cd /host/src/oc_erchef #Repeat for every erlang app
    - ./rebar3 upgrade
  - This will create an updated rebar.lock file.
  - cp rebar.lock /dotfiles #dotfiles folder is shared on host
  - On the host (mac)
    - cp chef-server/dev/dotfiles/rebar.lock chef-server/src/oc_erchef
  - Repeat the above steps for all the erlang apps.
    - As of 02/21 these are all of the rebar.lock files that need updating:
```
chef-server/src/bookshelf/rebar.lock
chef-server/src/oc_erchef/rebar.lock
chef-server/src/oc_bifrost/rebar.lock
chef-server/src/oc_erchef/apps/data_collector/rebar.lock
```
- Updating the erlang deps from the host machine
  - cd chef-server/src/oc_erchef
  - rm -fr \_build
  - ./rebar3 upgrade
  - rm -fr \_build # not removing this can cause strange build problems where omnibus builds pick up mac architecture libraries
  - The lockfile will be updated in place. .
  - Repeat above steps for all the erlang apps.
    - As of 02/21 these are all of the rebar.lock files that need updating:
```
chef-server/src/bookshelf/rebar.lock
chef-server/src/oc_erchef/rebar.lock
chef-server/src/oc_bifrost/rebar.lock
```

## Buildkite Artifacts (omnibus/adhoc)

Build artifacts are no longer automatically published to artifactory, in order to speed up pipeline runs in cases where artifacts aren't needed.  To publish an artifact in Artifactory's unstable channel, you must first set the Buildkite environment variable `PUBLISH_TO_ARTIFACTORY=true`. (**Buildkite web UI > New Build > Options** opens the Environment Variables settings).

It may be possible to have the artifacts for the nightlies published to artifactory by adding an environment variable to .expeditor/config.yml (untested):

```
diff --git a/.expeditor/config.yml b/.expeditor/config.yml
index 36962f703..ac772214e 100644
--- a/.expeditor/config.yml
+++ b/.expeditor/config.yml
@@ -43,6 +43,7 @@ pipelines:
  - omnibus/adhoc:
       definition: .expeditor/release.omnibus.yml
       env:
         - ADHOC: true
+        - PUBLISH_TO_ARTIFACTORY: true
   - post-promote:
       description: "Generate and upload release manifest"
       definition: .expeditor/post-promote.pipeline.yml
```
On occasion you may encounter a strange or unfamiliar error in the build phase. This may occur on just one platform. Sometimes this is caused by a stale cache that needs to be refreshed with the `EXPIRE_CACHE=true` setting.  You may also try clicking the `Clean checkout` box in the GUI.

An error message caused by a stale cache is depicted below (subsequently fixed with `EXPIRE_CACHE=true`):
```
[GitCache: preparation] I | 2021-12-20T06:26:13+00:00 | $ git -c core.autocrlf=false -c core.ignorecase=false --git-dir="/var/cache/omnibus/chef-server/cache/git_cache/opt/opscode" --work-tree="/opt/opscode" tag -f restore_here "preparation-cfb266e8545d283f3c6062a1e7eaa9f2c4b1ca45e729b4ff2b637fe59dc3972b-3"
                          I | 2021-12-20T06:26:13+00:00 | fatal: cannot update ref 'refs/tags/restore_here': trying to write ref 'refs/tags/restore_here' with nonexistent object 9db05b5933b4399f57467e20f5a2d691f77a740e
The following shell command exited with status 128:

    $ git -c core.autocrlf=false -c core.ignorecase=false --git-dir="/var/cache/omnibus/chef-server/cache/git_cache/opt/opscode" --work-tree="/opt/opscode" tag -f restore_here "preparation-cfb266e8545d283f3c6062a1e7eaa9f2c4b1ca45e729b4ff2b637fe59dc3972b-3"

Output:

    (nothing)

Error:

    fatal: cannot update ref 'refs/tags/restore_here': trying to write ref 'refs/tags/restore_here' with nonexistent object 9db05b5933b4399f57467e20f5a2d691f77a740e
```

## Manual LDAP setup

The easiest way to setup LDAP boxes is to use an Umbrella (Terraform) scenario.  A workflow for this is provided below.

1) `cd` to the appropriate directory.
`$ cd /umbrella/chef-server/scenarios/aws`

2) Setup your credentials, create the vpc
```
$ okta_aws --all
Fetching credentials for: chef-engineering
Assuming AWS role Okta_AdministratorAccess...
Temporary credentials stored in profile chef-engineering
Credentials expire in 12 hours
$ make create-vpc
```
3) Run the external-openldap scenario, using this template.  Fill-in the blanks with your chosen platform, chef-server version, etc.  If you need to use manage, leave `ENABLE_ADDON_CHEF_MANAGE=true` as manage is not automatically installed.  Example:

$ PLATFORM=ubuntu-18.04 INSTALL_VERSION=14.11.21 UPGRADE_VERSION=14.11.21 SCENARIO=external-openldap ENABLE_ADDON_PUSH_JOBS=false ENABLE_GATHER_LOGS_TEST=false ENABLE_PEDANT_TEST=false ENABLE_PSQL_TEST=false ENABLE_SMOKE_TEST=false ENABLE_IPV6=false ENABLE_ADDON_CHEF_MANAGE=true MANAGE_VERSION=3.2.20 make apply | tee /tmp/out

4) LDAP sets up multiple boxes.  If you captured the scenario output with `tee /tmp/out` above, use the command below to discover the IP addresses for the boxes, and to determine which box is which (chef-server, or ldap). The box labeled in the text to the left as 'ldap_config' is the LDAP box.  The box labeled either 'chef_server_config' or 'chef_server_test' is the chef-server box.  Use the chef-server address for the manage web GUI.
```
$ grep Host: /tmp/out
null_resource.ldap_config (remote-exec):   Host: ec2-34-219-234-185.us-west-2.compute.amazonaws.com
null_resource.chef_server_config (remote-exec):   Host: ec2-35-87-152-36.us-west-2.compute.amazonaws.com
null_resource.chef_server_config (remote-exec): > Host: packages.chef.io
null_resource.chef_server_config (remote-exec):   Host: ec2-35-87-152-36.us-west-2.compute.amazonaws.com
null_resource.chef_server_config (remote-exec): > Host: packages.chef.io
null_resource.chef_server_config (remote-exec):   Host: ec2-35-87-152-36.us-west-2.compute.amazonaws.com
null_resource.chef_server_test (remote-exec):   Host: ec2-35-87-152-36.us-west-2.compute.amazonaws.com
null_resource.chef_server_test (remote-exec):   Host: ec2-35-87-152-36.us-west-2.compute.amazonaws.com
null_resource.chef_server_test (remote-exec):   Host: ec2-35-87-152-36.us-west-2.compute.amazonaws.com
```

Notes.

If you've logged into a box via CLI, you can confirm which box it is by using this trick.  If it's the chef-server box, it will respond to `ctl` commands.  If it's the LDAP box, it won't:
```
$ chef-server-ctl status
run: bookshelf: (pid 18881) 405s; run: log: (pid 18478) 425s
run: elasticsearch: (pid 18862) 405s; run: log: (pid 18273) 468s
run: nginx: (pid 2572) 179s; run: log: (pid 18686) 413s
run: oc_bifrost: (pid 18736) 407s; run: log: (pid 18142) 480s
run: oc_id: (pid 18782) 406s; run: log: (pid 18183) 473s
run: opscode-erchef: (pid 18897) 404s; run: log: (pid 18601) 419s
run: postgresql: (pid 18722) 407s; run: log: (pid 17655) 492s
run: redis_lb: (pid 2278) 239s; run: log: (pid 18948) 404s
```
Note that the umbrella scenario automatically creates a `user1` account [ldap? chef-server?] with password `password`.

If you are using the manage web GUI along with CLI, for a quick knife setup click on the Administration tab, and click on 'Generate Knife Config' on the left.  Copy the 'config.rb' file to your current directory on the chef-server box, and issue the following:
```
alias knife='/opt/opscode/bin/knife'
export EDITOR=vim # edit this line to taste
knife ssl fetch
knife ssl check
```

