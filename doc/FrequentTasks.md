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

## Updating Ruby Gems

The following Gemfile.locks need to be updated to do a complete
sweep. We strongly recommend using bundler 1.17.x until bundler 2.1 is
out and stable.

For personal sanity, remove all .bundle and vendor directories before
starting and after you complete the updates. 

- oc-chef-pedant/Gemfile.lock
- src/oc-id/Gemfile.lock
  Due to the rails version, we've locked a lot of dependencies. Make
  sure we can build and run oc-id before merging to master.
  You'll need the libsqlite3-dev library if doing this on Ubuntu

- src/opscode-expander/Gemfile.lock
- src/oc\_erchef/apps/chef_objects/priv/depselector\_rb/Gemfile.lock
- src/chef-server-ctl/Gemfile.lock
- src/oc_bifrost/oc-bifrost-pedant/Gemfile.lock

Note:
- omnibus/Gemfile does *not* have a committed Gemfile.lock. We float on
  omnibus-software deliberately to keep up with the latest build
  dependencies.

Note:
- It's best to run
    bundle install --binstubs --path=vendor/bundle
    bundle update
  in the appropriate directories to keep your main ruby install
  clean. However do not forget to clean those up before doing an
  omnibus build, as the files there (especially .so files) can cause
  strange build failures, especially during the final consistency checks.

Note:
- On the dev-vm /dotfiles dir is mounted from your host. It is useful for
copying files back from the dev-vm to the host.
- The ./sync utility is used to copy files from the host into dev-vm.

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

- Build Chef Server, and do a chef-server-ctl reconfigure. Fix any new
  warnings about deprecations and the like.

## Updating rebar3

- It is helpful to periodically update the copy of the rebar3 tooling
  that is included with Chef Infra Server.
- The following erlang apps need to be updated:
  - bookshelf
  - chef-mover
  - oc_erchef
  - oc_bifrost
- Check for the current rebar3 version with `./rebar3 --version`
- Latest version for rebar3 can be found at: https://www.rebar3.org/
- Download and update the current rebar3 executable with it.

## Updating the erlang dependencies using rebar3

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
    - ./rebar3 update
  - This will create an updated rebar.lock file.
  - cp rebar.lock /dotfiles #dotfiles folder is shared on host
  - On the host (mac)
    - cp chef-server/dev/dotfiles/rebar.lock chef-server/src/oc_erchef
  - Repeat the above steps for all the erlang apps.
- Updating the erlang deps from the host machine
  - cd chef-server/src/oc_erchef
  - rm -fr \_build
  - ./rebar3 update
  - rm -fr \_build # not removing this can cause strange build problems where omnibus builds pick up mac architecture libraries
  - The lockfile will be updated in place. .
  - Repeat above steps for all the erlang apps.
