require 'partybus/schema_migrator'
require 'partybus/service_manager'
require 'partybus/command_runner'
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
        if role == "backend" && Partybus.config.bootstrap_server
          if db_up?
            log("\tDatabase Up")
            perform_schema_upgrade(version)
          else
            log <<EOF
****
ERROR: Database is not running on bootstrap server.
****
You must run database schema migrations on the bootstrap server, and the
bootstrap server must be the HA-master at the time of the migration.
If the bootstrap server is not currently the HA-master, please see the
Private Chef documentation for issuing a graceful transition of the HA
cluster.
****
EOF
            exit 1
          end
        elsif role == "standalone"
          if db_up?
            perform_schema_upgrade(version)
          else
            log <<EOF
****
ERROR: Database is not running.
****
The database must be running to execute schema migrations. Please start
the database and re-run private-chef-ctl upgrade.
****
EOF
            exit 1
          end
        else
          log("\tSkipping Schema Upgrade")
        end
      end

      def restart_service(service_name)
        log("\tRestarting Service #{service_name}")
        service_manager = Partybus::ServiceManager.new
        service_manager.restart_service(service_name)
      end

      def start_service(service_name)
        log("\tStarting Service #{service_name}")
        service_manager = Partybus::ServiceManager.new
        # 100 seconds sleep time by default
        service_manager.start_service(service_name, 60)
      end

      def start_services(service_array)
        log("\tStarting Services #{service_array}")
        service_manager = Partybus::ServiceManager.new
        # 100 seconds sleep time by default
        service_manager.start_services(service_array, 60)
      end

      def stop_service(service_name)
        log("\tStopping Service #{service_name}")
        service_manager = Partybus::ServiceManager.new
        service_manager.stop_service(service_name, 30)
      end

      def stop_services(service_array)
        log("\tStopping Services #{service_array}")
        service_manager = Partybus::ServiceManager.new
        service_manager.stop_services(service_array, 60)
      end

      def run_command(command, options={})
        log("\tRunning Command: #{command} with options #{options.inspect}")
        runner = Partybus::CommandRunner.new
        runner.run_command(command, options)
      end

      def must_be_data_master
        if !Partybus.config.is_data_master
          log <<EOF
****
ERROR: 
****
The bootstrap server must be the HA-master at the time of upgrade.
If the bootstrap server is not currently the HA-master, please see the
Private Chef documentation for issuing a graceful transition of the HA
cluster.
****
EOF
          exit 1
        end
      end

      def migrate

      end

      private # private really doesn't do anything when we #instance_eval

      def db_up?
        db_service = Partybus.config.database_service_name
        system("private-chef-ctl status #{db_service}")
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
