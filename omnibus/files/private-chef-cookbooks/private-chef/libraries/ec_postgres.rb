class EcPostgres
  # Provides a superuser connection to the specified database
  def self.with_connection(node, database = 'template1', opts = {})
    require 'pg'
    postgres = node['private_chef']['postgresql'].merge(opts)
    connection = nil
    retries = 5
    begin
      connection = ::PGconn.open('user' => postgres['db_superuser'],
                                 'host' => postgres['vip'],
                                 'password' => postgres['db_superuser_password'],
                                 'port' => postgres['port'],
                                 'dbname' => database)
    rescue e
      if retries > 0
        retries -= 1
        Chef::Log.warn "Error connecting to postgresql: #{e.message}, retrying after 1s sleep. #{retries} retries remaining."
        sleep 1
        retry
      else
        Chef::Log.warn "Error from postgresql: #{e.message}, retries have been exhausted."
        raise
      end
    end

    begin
      yield connection
    ensure
      connection.close
    end
  end

  def self.with_service_connection(node, database, service_name)
    service = node['private_chef'][service_name]
    with_connection(node, database,
                    db_superuser: service['sql_user'],
                    db_superuser_password: service['sql_password']) do |connection|
      yield connection
    end
  end

  # By default, with_connection will create a superuser connection over tcp to the specified database.
  # This method will create a unix socket connection to a local database instance. This should only be used
  # to the extent required to set configure tcp access and set a password for the superuser.
  def self.with_local_connection(node, database = 'template1')
    require 'pg'
    postgres = node['private_chef']['postgresql']
    as_user(postgres['username']) do
      connection = ::PGconn.open('dbname' => database, :port => postgres['port'])
      begin
        yield connection
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
