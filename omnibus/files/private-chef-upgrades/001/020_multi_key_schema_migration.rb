define_upgrade do
  require "pg"
  # sql user info will be in one of two places - under 'postgresql' or under 'opscode-erchef' in more
  # recent versions. If someone is upgrading from an earlier versions, it may not yet
  # have been moved to its new location.
  if Partybus.config.bootstrap_server
    must_be_data_master
    run_sqitch("@2.5.3", "@1.0.4")

    running_config = JSON.parse(File.read("/etc/opscode/chef-server-running.json"))
    pc = running_config['private_chef']
    keyname = pc['opscode-erchef'].has_key?('sql_user') ? 'opscode-erchef' : 'postgresql'
    connection = ::PGconn.open('user' => pc[keyname]['sql_user'],
                               'host' => pc['postgresql']['vip'],
                               'password' => pc[keyname]['sql_password'],
                               'port' => pc['postgresql']['port'],
                               'dbname' => 'opscode_chef')
    begin
      connection.exec("select migrate_keys();")
    ensure
      connection.close
    end
  end
end
