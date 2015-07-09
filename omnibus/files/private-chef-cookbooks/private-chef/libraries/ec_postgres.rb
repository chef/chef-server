class EcPostgres
  # Provides a superuser connection to the specified database
  def self.with_connection(node, database = 'template1')
    require 'pg'
    postgres = node['private_chef']['postgresql']
    if postgres['external']
      connection = ::PGconn.open('user' => postgres['db_superuser'],
                                'host' => postgres['vip'], 'password' => postgres['db_superuser_password'],
                                'port' => postgres['port'], 'dbname' => database)
      begin
        yield connection
      ensure
        connection.close
      end
    else
      # Local administrative connections are still done using the
      # dedicated postgresql user over pipe.
      # Use of Proc.new will pass our implicit block into with_local_connection
      with_local_connection(node, database, &Proc.new)
    end
  end

  # In our target state for chef-server external postgres support, this will be used only once to assign a password
  # to the admin user and will be used explicitly. From that point forward, the usual 'with_connection'
  #  will be used, and it will support only tcp-based connections.
  def self.with_local_connection(node, database = 'template1', &block)
     postgres = node['private_chef']['postgresql']
     as_user(postgres['username']) do
       connection = ::PGconn.open('dbname' => database)
       begin
         block.call(connection)
       ensure
         connection.close
       end
     end
  end
  private

  def self.as_user(user)
    # Find the user in the password database.
    u = (user.is_a? Integer) ? Etc.getpwuid(user) : Etc.getpwnam(user)

    old_process_euid = Process.euid
    Process::UID.eid = u.uid
    begin
      yield
    ensure
      Process::UID.eid = old_process_euid
    end
  end
end
