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
- [ ] Should we default quickstart ercehf to launch in the background to
      avoid another ssh session for more work? One the one hand, simpler
      to start, on the other hand it's nice to see the immediate errors/info
      feedback when you get started...
- [ ] It would be simple to do automatic git checkout of project dependencies on
      requested load, but ultimately not helpful because we're using
      vagrant rsync - the sync is one way, and the checked out projects won't
      show up on the host.  One option to do this is to *additional* load in
      the host location as a standard vbox share  and mount it to /host_rw or something.
      Convoluted but it would work.
- [ ] not really any support for ruby project deps, something that we
      will want if we expand this to support the things that layer atop chef
      server (manage, etc)
