require 'partybus/schema_migrator'
require 'partybus/service_restarter'
require 'partybus/migration_api/v1'

module Partybus
  module UpgradeAPI
    class V1

      include Logger

      def initialize(&block)
        self.instance_eval(&block)
      end

      def maintenance_mode

      end

      def upgrade_schema_to(version)
        role = Partybus.config.private_chef_role
        log("\tPrivate Chef Role: #{role}")
        if ["backend", "standalone"].include? role
          log("\tUpgrading Schema to Version #{version}")
          migrator = Partybus::SchemaMigrator.new
          migrator.migrate_to(version)
        else
          log("\tSkipping Schema Upgrade")
        end
      end

      def restart_service(service_name)
        log("\tRestarting Service #{service_name}")
        restarter = Partybus::ServiceRestarter.new
        restarter.restart_service(service_name)
      end

      def migrate

      end
    end
  end
end
