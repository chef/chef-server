require 'partybus/schema_migrator'
require 'partybus/migration_api/v1'

module Partybus::UpgradeAPI
  class V1

    def initialize(&block)
      self.instance_eval(&block)
    end

    def maintenance_mode

    end

    def upgrade_schema_to(version)
      migrator = Partybus::SchemaMigrator.new
      migrator.migrate_to(version)
    end

    def restart_service

    end

    def migrate

    end
  end
end
