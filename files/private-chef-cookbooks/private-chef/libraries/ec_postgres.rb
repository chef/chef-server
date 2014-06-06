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

    # Fork the child process. Process.fork will run a given block of code
    # in the child process.
    pid = fork do
      begin
        Process::UID.change_privilege(u.uid)
        yield
      rescue
        STDERR.puts $!
        STDERR.puts $!.backtrace.join("\n")
        raise
      end
    end
    pid, status = Process.waitpid2(pid)
    if status.exitstatus != 0
      raise "Error during as_user!  See log for details"
    end
  end

end
