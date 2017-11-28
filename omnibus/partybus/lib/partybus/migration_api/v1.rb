require 'partybus/schema_migrator'
require 'partybus/service_manager'
require 'partybus/migration_util'
require 'partybus/command_runner'
require 'partybus/migration_api/v1'

module Partybus
  module UpgradeAPI
    class V1

      include Logger

      attr_accessor :eval_result

      def initialize(&block)
        @eval_result = self.instance_eval(&block)
      end

      def maintenance_mode

      end

      def upgrade_schema_to(version)
        role = Partybus.config.private_chef_role
        log("\tPrivate Chef Role: #{role}")
        if role == "backend" && Partybus.config.bootstrap_server
          if !perform_schema_upgrade(version)
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
          if !perform_schema_upgrade(version)
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

      def force_restart_service(service_name)
        log("\tRestarting Service #{service_name}")
        service_manager = Partybus::ServiceManager.new
        service_manager.force_restart_service(service_name, 15)
      end

      def start_service(service_name)
        log("\tStarting Service #{service_name}")
        service_manager = Partybus::ServiceManager.new
        # 100 seconds sleep time by default
        service_manager.start_service(service_name, 15)
      end

      def start_services(service_array)
        log("\tStarting Services #{service_array}")
        service_manager = Partybus::ServiceManager.new
        # 100 seconds sleep time by default
        service_manager.start_services(service_array, 15)
      end

      def stop_service(service_name)
        log("\tStopping Service #{service_name}")
        service_manager = Partybus::ServiceManager.new
        service_manager.stop_service(service_name, 10)
      end

      def stop_services(service_array)
        log("\tStopping Services #{service_array}")
        service_manager = Partybus::ServiceManager.new
        service_manager.stop_services(service_array, 10)
      end

      def run_command(command, options={})
        log_msg = options[:log_msg] || "Running Command: #{command} with options #{options.inspect}"
        log("\t#{log_msg}")
        runner = Partybus::CommandRunner.new
        runner.run_command(command, options)
      end

      # Caller may use 'opts' to override :username :password,
      # :database, and/or :path.
      def run_sqitch(target, service, opts = {})
        options = default_opts_for_service(service).merge(opts)
        command = <<-EOM.gsub(/\s+/," ").strip!
          sqitch --engine pg
            --db-name #{options[:database]}
            --db-host #{Partybus.config.postgres['vip']}
            --db-port #{Partybus.config.postgres['port']}
            --db-user #{options[:username]}
            --top-dir /opt/opscode/embedded/service/#{options[:path]}
            deploy #{target} --verify
        EOM
        run_command(command, env: {"PGPASSWORD" => options[:password]})
      end

      CHECK_SQITCH_SQL="SELECT 1 FROM sqitch.tags WHERE tag=$1"
      def check_sqitch(target, service, opts = {})
        options = default_opts_for_service(service).merge(opts)
        c = pg_conn_for(service, opts)
        result = c.exec(CHECK_SQITCH_SQL, [target])
        result.cmd_tuples == 1
      ensure
        c.close if c
      end

      def pg_conn_for(service, opts = {})
        require 'pg'
        options = default_opts_for_service(service).merge(opts)
        ::PGconn.open({'user' => options[:username],
                      'password' => options[:password],
                      'dbname' => options[:database],
                      'host' => Partybus.config.postgres['vip'],
                      'port' => Partybus.config.postgres['port']})
      end

      def default_opts_for_service(service)
        username = Partybus.config.postgres['db_superuser']
        password = Partybus.config.secrets['postgresql']['db_superuser_password'].value
        path = "#{service}/schema"
        case service
        when 'oc_erchef'
          database = 'opscode_chef'
          path = 'opscode-erchef/schema'
        when 'oc_bifrost'
          database = 'bifrost'
          path = 'oc_bifrost/db'
        when 'bookshelf'
          database = 'bookshelf'
        end

        {username: username, password: password,
         database: database, path: path }
      end

      def clean_mover_logs
        log("\tCleaning migration related logs to prep for new migration")
        migration_util = Partybus::MigrationUtil.new
        migration_util.clean_mover_logs
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

      def ensure_db_up
        retries = 0
        until db_up? || retries == 5
          log("\tAttempting to start #{Partybus.config.database_service_name}")
          start_service(Partybus.config.database_service_name)
          retries += 1
          sleep retries
        end
        db_up?
      end

      def perform_schema_upgrade(version)
        return false unless ensure_db_up

        log("\tUpgrading Schema to Version #{version}")
        migrator = Partybus::SchemaMigrator.new
        migrator.migrate_to(version)
        stop_service(Partybus.config.database_service_name)
      end
    end
  end
end
