  define_upgrade do
    require 'mixlib/shellout'
    if Partybus.config.bootstrap_server
      must_be_data_master

      # Make sure API is down
      stop_services(["nginx", "opscode-erchef"])
      start_services(["postgresql"])

      sleep 30

      # Run reindex for postgresql 13
      log "Reindexing postgresql..."
      cmd = Mixlib::ShellOut.new("/opt/opscode/embedded/bin/reindexdb --all", :user => 'postgres')
      cmd.run_command

    end
  end
