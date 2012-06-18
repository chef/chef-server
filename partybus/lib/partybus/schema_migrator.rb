require 'sequel'
Sequel.extension :migration
require 'logger'

class Partybus::SchemaMigrator

  def initialize(options={})
    @db = options[:db] || database_from_config
  end

  def self.config
    Partybus.config
  end

  def config
    self.class.config
  end

  def migrate_to(version)
    if version > current_schema_version
      Sequel::Migrator.apply(@db, config.database_migration_directory, version)
    else
      # TODO: log something
    end
  end

  def current_schema_version
    begin
      @db[:schema_info].first[:version]
    rescue # TODO: Sequel::DatabaseError
      0
    end
  end

  private

  def database_from_config
    Sequel.connect(config.database_connection_string, :loggers => [Logger.new(STDOUT)])
  end

end
