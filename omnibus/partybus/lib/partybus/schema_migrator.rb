require 'sequel'
Sequel.extension :migration
require 'logger'

class Partybus::SchemaMigrator

  def self.config
    Partybus.config
  end

  def config
    self.class.config
  end

  def migrate_to(version)
    if version > current_schema_version
      with_database do |db|
        Sequel::Migrator.apply(db, config.database_migration_directory, version)
      end
    else
      # TODO: log something
    end
  end

  def current_schema_version
    begin
      with_database do |db|
        db[:schema_info].first[:version]
      end
    rescue # TODO: Sequel::DatabaseError
      0
    end
  end

  private

  def as_user(user)
    if user
      # Find the user in the password database.
      u = (user.is_a? Integer) ? Etc.getpwuid(user) : Etc.getpwnam(user)

      old_process_euid = Process.euid
      Process::UID.eid = u.uid
      begin
        yield
      ensure
        Process::UID.eid = old_process_euid
      end
    else
      yield
    end
  end

  def with_database
    as_user(config.database_unix_user) do
      db = Sequel.connect(config.database_connection_string, :loggers => [Logger.new(STDOUT)])
      yield db
    end
  end

end
