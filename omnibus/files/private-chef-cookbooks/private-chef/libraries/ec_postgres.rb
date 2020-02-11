class EcPostgres
  # Provides a superuser connection to the specified database
  def self.with_connection(node, database = 'template1', opts = {})
    require 'pg'
    postgres = node['private_chef']['postgresql'].merge(opts)
    postgres['db_superuser_password'] ||= PrivateChef.credentials.get('postgresql', 'db_superuser_password')
    connection = nil

    # Some callers expect failure - this gives the option to suppress
    # error logging to avoid confusing output.
    if opts['silent']
      silent = true
      # If a caller specfies silent, it means they anticipate an error to be
      # likely. Don't force over a minute of retries in that case.
      retries = opts['retries'] || 1
    else
      silent = false
      retries = opts['retries'] || 5
    end
    max_retries = retries
    begin
      connection = PG::Connection.open('user' =>     postgres['db_connection_superuser'] || postgres['db_superuser'],
                                       'host' =>     postgres['vip'],
                                       'password' => postgres['db_superuser_password'],
                                       'port' =>     postgres['port'],
                                       'sslmode' =>  postgres['sslmode'],
                                       'dbname' =>   database)
    rescue => e
      if retries > 0
        sleep_time = 2**((max_retries - retries))
        retries -= 1
        unless silent
          Chef::Log.warn "Error from postgresql: #{e.message.chomp}. Retrying after #{sleep_time}s. Retries remaining: #{retries + 1}"
        end
        sleep sleep_time
        retry
      else
        unless silent
          Chef::Log.error "Error from postgresql: #{e.message.chomp}. Retries have been exhausted."
        end
        raise
      end
    end

    begin
      yield connection
    ensure
      connection.close
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
