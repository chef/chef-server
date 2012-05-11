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
        if role == "backend"
          if db_up?
            log("\tDatabase Up")
            perform_schema_upgrade(version)
          else
            log("\tDatabase Down")
            log("\tSkipping Schema Upgrade")
          end
        elsif role == "standalone"
          perform_schema_upgrade(version)
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

      private # private really doesn't do anything when we #instance_eval

      def db_up?
        db_service = Partybus.config.database_service_name
        system("private-chef-ctl #{db_service} status")
        exit_status = $?.exitstatus
        exit_status == 0
      end

      def perform_schema_upgrade(version)
        log("\tUpgrading Schema to Version #{version}")
        migrator = Partybus::SchemaMigrator.new
        migrator.migrate_to(version)
      end
    end
  end
end
