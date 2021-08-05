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
      #run_command("/opt/opscode/embedded/bin/reindexdb --all", :user => 'opscode-pgsql')
      running_config = JSON.parse(File.read("/etc/opscode/chef-server-running.json"))
      pc = running_config['private_chef']
      keyname = pc['opscode-erchef'].has_key?('sql_user') ? 'opscode_erchef' : 'opscode-pgsql'
      connection = ::PG::Connection.open('user' => pc[keyname]['sql_user'],
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
