  define_upgrade do
    require 'pg'
    if Partybus.config.bootstrap_server
      must_be_data_master

      # Make sure API is down
      stop_services(["nginx", "opscode-erchef"])
      start_services(["postgresql"])

      sleep 30

       # Run reindex for postgresql 13
      log "Reindexing postgreSQL..."
      #run_command("/opt/opscode/embedded/bin/reindexdb --all", :user => 'opscode-pgsql')
      running_config = JSON.parse(File.read("/etc/opscode/chef-server-running.json"))
      pc = running_config['private_chef']
      connection = ::PG::Connection.open('user' => pc['opscode-erchef']['sql_user'],
                                 'host' => pc['postgresql']['vip'],
                                 'password' => Partybus.config.secrets.get('opscode_erchef', 'sql_password'),
                                 'port' => pc['postgresql']['port'],
                                 'dbname' => 'opscode_chef')
      begin
        connection.exec("REINDEX DATABASE opscode_chef")
      ensure
        connection.close
      end
    end
  end
