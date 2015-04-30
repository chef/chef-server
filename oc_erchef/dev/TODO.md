- [ ] MOTD with some useful instructions and available commands
- [ ] populate command to load test users and orgs
- [ ] simple merge of vm settings in config.yml and default.yml files so that
      packages and vm config can be overridden.
- [ ] Once that's done, update config.yml  with basic skeleton to show what can be done,
      then add to .gitignore
- [ ] verify `bookshelf` and `oc_bifrost` work without further
      modification
- [ ] option to auto-modify /etc/hosts to add thte vm name
      Also ensure DNS for named host resolves to external IP and
      everything works with this.
- [ ] for each project setting - or just at vm level? , add forward_ports. Note that user knife
      config, etc should work from in the vm or from the host.
- [ ] refactor the crap out of projects.rb...
- [ ] Should we default quickstart ercehf to launch in the background to
      avoid another ssh session for more work? One the one hand, simpler
      to start, on the other hand it's nice to see the immediate errors/info
      feedback when you get started...
- [x] clone and load project deps (erlang)
- [ ] not really any support for ruby project deps, something that we
      will want if we expand this to support the things that layer atop chef
      server (manage, etc)
  [ ] why doesn't NFS file share work, at least with linux host?
  [ ] dvm run oc-chef-pedant appears to run as if '--all' were specified
      by default, instead of '--smoke'

