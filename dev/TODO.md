- [ ] bug: dvm gem mount gets undone after halt/up
- [ ] populate command to load test users and orgs
- [ ] use runsv 'down' file to cleanly disable loaded services even
      across chef-server-ctl reconfigure
- [ ] simple merge of vm settings in config.yml and default.yml files so that
      packages and vm config can be overridden.
- [ ] Once that's done, update config.yml  with basic skeleton to show what can be done,
      then add to .gitignore
- [ ] we're all in one place - can we use and include a ../concrete.mk
      to simplify keeping things up to date?
- [ ] add 'sync' erlang dependency to chef-mover
- [ ] mover to relx and concrete
- [ ] option to auto-modify /etc/hosts to add the vm name
      Also ensure DNS for named host resolves to external IP and
      everything works with this.
- [ ] for each project setting - or just at vm level? , add forward_ports. Note that user
      knife config, etc should work from in the vm or from the host.
- [ ] Should we default quickstart ercehf to launch in the background to
      avoid another ssh session for more work? One the one hand, simpler
      to start, on the other hand it's nice to see the immediate errors/info
      feedback when you get started...
- [ ] not really any support for ruby project deps, something that we
      will want if we expand this to support the things that layer atop chef
      server (manage, etc)
- [ ] why doesn't NFS file share work for intensive IO (git pulls,
      builds), at least with linux host?
- [ ] dvm run oc-chef-pedant appears to run as if '--all' were specified
      by default, instead of '--smoke' (this may be a change in behavior
      of pedant itself due to other recent changes)
- [ ] modify auto clone/load of erlang project deps so that it won't clutter up
      the chef-server dir on the host.
- [ ] add oc_id supprt
- [ ] add indexer support (is this worth it?)
- [ ] ruby project dep loading support. Should just be able to reference
      gemfile.lock
- [ ] can ruby services (oc-id) support hot-loading of changes as erlang
      projects can? If not, can we fake it?
- [ ] re-add support for compiling dependencies after linking them in,
      instead of relying on the hot loading - hot loading doesn't work
      if the service isn't started (and it won't work for projects that
      don't have sync...)
- [x] bookshelf, bifrost: don't run unit on devrel, and add proper usage
      of relx devmode for clean symlinks
- [x] verify `bookshelf` works as-is
- [x] clone and load project deps (erlang)
- [x] refactor the crap out of projects.rb...
- [x] verify `oc_bifrost` works as-is
- [x] add 'sync' erlang dependency to bookshelf, oc_bifrost
- [x] MOTD with some useful instructions and available commands
