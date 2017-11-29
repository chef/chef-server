require 'pg'

module Partybus
  class MigrationState
    # NOTE(ssd) 2017-11-28: Why not use sqitch?
    #
    # We are inside the tool responsible for deciding whether or not
    # we need to run sqitch.
    #
    # I'm open to other ways of managing this though, as it does feel
    # a little ugly.
    MIGRATION_STATE_SCHEMA =<<SQL
CREATE TABLE IF NOT EXISTS partybus_migration_state(
    name        TEXT PRIMARY KEY,
    value       TEXT,
    updated_at  TIMESTAMP WITHOUT TIME ZONE NOT NULL
)
SQL

    # NOTE(ssd) 2017-11-28: Why not a table level lock?
    #
    # The table might not exist yet and we also want to lock against
    # concurrent initialization of this data.  Or at least, that is
    # what I'm telling myself at the moment. Maybe a table-level lock
    # would be better.
    MIGRATION_STATE_LOCK_ID = 21193.freeze
    LOCK_ACQUIRE_SQL = "SELECT pg_try_advisory_lock(#{MIGRATION_STATE_LOCK_ID})".freeze
    LOCK_RELEASE_SQL = "SELECT pg_advisory_unlock(#{MIGRATION_STATE_LOCK_ID})".freeze

    INIT_QUERY = "SELECT true as initialized FROM partybus_migration_state WHERE name = 'migration-level'".freeze
    INSERT_SQL = "INSERT INTO partybus_migration_state VALUES ('migration-level', $1, NOW())".freeze
    UPDATE_SQL = "UPDATE partybus_migration_state SET value = $1 WHERE name = 'migration-level'".freeze

    def initialize
      @connection = init_db_connection
      lock # Released on session close
    end

    def init(migration)
      init_schema
      insert_from_migration(migration)
    end

    def load_or_upgrade(legacy_migration_state_file_path = Partybus.config.legacy_migration_state_file)
      if initialized?
        load_from_db
        true
      elsif legacy_state_valid?(legacy_migration_state_file_path)
        log "Valid legacy migration-state found. Creating db entry from legacy state."
        init_schema
        insert_from_file(legacy_migration_state_file_path)
        load_from_db
        true
      else
        false
      end
    end

    def apply(migration)
      update_from_migration(migration)
    end

    private

    def init_schema
      @connection.exec(MIGRATION_STATE_SCHEMA)
    end

    def initialized?
      r = @connection.exec(INIT_QUERY)
      r.first['initialized'] == 't'
    rescue
      false
    end

    def insert_from_file(path)
      data = IO.read(path)
      @connection.exec(INSERT_SQL, [data])
    end

    def insert_from_migration(migration)
      data = {:major => migration.major, :minor => migration.minor}.to_json
      @connection.exec(INSERT_SQL, [data])
    end

    def update_from_migration(migration)
      data = {:major => migration.major, :minor => migration.minor}.to_json
      @connection.exec(UPDATE_SQL, [data])
    end

    def load_from_db
      res = @connection.exec("SELECT * FROM partybus_migration_state WHERE name = 'migration-level'")
      @major, @minor = parse_migration_level_data(res.first['value'])
    end

    def lock
      res = @connection.exec(LOCK_ACQUIRE_SQL)
      if res.first['pg_try_advisory_lock'] == "t"
        true
      else
        raise "Cannot lock migration state.  Another upgrade in progress?"
      end
    end

    # The lock gets released when our session gets closed so no one
    # calls this.
    def unlock
      @connection.exec(LOCK_RELEASE_SQL)
    end

    def parse_migration_level_data(data)
      d = JSON.parse(data)
      [d["major"].to_i, d["minor"].to_i]
    end

    def legacy_state_valid?(path)
      major, minor = parse_migration_level_data(IO.read(path))
      major && minor
    rescue
      false
    end

    def init_db_connection
      dbname = 'opscode_chef'
      host = Partybus.config.postgresql_host
      port = Partybus.config.postgresql_port
      username = Partybus.config.postgresql_user
      password = Partybus.config.secrets['postgresql']['db_superuser_password'].value

      ::PGconn.open({'user' => username,
                     'password' => password,
                     'dbname' => dbname,
                     'host' => host,
                     'port' => port})
    end
  end
end
