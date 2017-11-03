class EcPostgres
  # Provides a superuser connection to the specified database
  def self.with_connection(database = 'template1', opts = {})
    require 'pg'
    postgres = {}

{{#if bind.database}}
  {{#eachAlive bind.database.members as |member|}}
    {{#if @last}}
    postgres['vip']="{{member.sys.ip}}"
    postgres['port']="{{member.cfg.port}}"
    postgres['db_superuser_password']="{{member.cfg.superuser_name}}"
    postgres['db_superuser_password']="{{member.cfg.superuser_password}}"
    # DB="opscode_chef"
    {{/if}}
  {{/eachAlive}}
{{else}}
    postgres['vip']="{{cfg.postgresql.vip}}"
    postgres['port']="{{cfg.postgresql.port}}"
    postgres['db_superuser_password']="{{cfg.sql_user}}"
    postgres['db_superuser_password']="{{cfg.sql_password}}"
    # DB="opscode_chef"
{{/if}}

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
      connection = ::PGconn.open('user' => postgres['db_superuser'],
                                 'host' => postgres['vip'],
                                 'password' => postgres['db_superuser_password'],
                                 'port' => postgres['port'],
                                 'dbname' => database)
    rescue => e
      if retries > 0
        sleep_time = 2**((max_retries - retries))
        retries -= 1
        unless silent
          puts "Error from postgresql: #{e.message.chomp}. Retrying after #{sleep_time}s. Retries remaining: #{retries + 1}"
        end
        sleep sleep_time
        retry
      else
        unless silent
          puts "Error from postgresql: #{e.message.chomp}. Retries have been exhausted."
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
