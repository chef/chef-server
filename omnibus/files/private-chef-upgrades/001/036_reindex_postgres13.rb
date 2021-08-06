define_upgrade do
  if Partybus.config.bootstrap_server
    must_be_data_master
    # Make sure API is down
    stop_services(%w(nginx opscode-erchef))
    start_services(['postgresql'])
    sleep 30
    # Run reindex for postgresql 13
    log 'Reindexing postgreSQL...'
    run_command("sudo -H -u  opscode-pgsql bash -c \'/opt/opscode/embedded/bin/reindexdb --all\'")
  end
end
