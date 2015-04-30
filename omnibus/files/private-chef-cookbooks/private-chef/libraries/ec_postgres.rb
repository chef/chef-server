class EcPostgres
  def self.with_connection(node, database = 'template1')
    require 'pg'
    as_user(node['private_chef']['postgresql']['username']) do
      connection = ::PGconn.open('dbname' => database)
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
