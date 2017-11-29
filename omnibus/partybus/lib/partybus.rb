module Partybus

  def self.config
    @config ||= Config.new
  end

  def self.configure(&block)
    yield(config)
  end

  class Config

    SECRETS_FILE = "/etc/opscode/private-chef-secrets.json"
    RUNNING_CONFIG_FILE = "/etc/opscode/chef-server-running.json"

    # Actively used configuration
    attr_accessor :bootstrap_server
    attr_accessor :is_data_master
    attr_accessor :partybus_migration_directory
    attr_writer :legacy_migration_state_file
    attr_writer :postgresql_user
    attr_writer :postgresql_port
    attr_writer :postgresql_host

    # Referenced but defunct configuration
    #
    # These are referenced from the code or the migrations but are not
    # likely to be used since they are only used by very old
    # migrations that we don't technically support. These can likely
    # be removed once we do
    #
    # https://github.com/chef/chef-server/pull/721
    #
    # or something similar.
    #
    attr_accessor :database_connection_string
    attr_accessor :database_unix_user
    attr_accessor :database_migration_directory
    attr_accessor :database_service_name
    attr_accessor :private_chef_role
    attr_accessor :couchdb_data_dir

    # Unused config. We keep these attr_writer's to support reading
    # older configuration files.
    attr_writer :migration_state_file

    # Just in case we *really* need to run against older configuration
    # files and can't run a reconfigure for some reason, we provide
    # some accessors here that check two possible locations for
    # recently moved configuration values.
    #
    # TODO(ssd) 2017-11-29: Do we really need to do this?  It would be nice
    # to remove most references to "running_server" from this tool.
    #
    def postgresql_port
      @postgresql_port || running_server['private_chef']['postgresql']['port']
    end

    def postgresql_host
      @postgresql_host || running_server['private_chef']['postgresql']['vip']
    end

    def postgresql_user
      @postgresql_user || running_server['private_chef']['postgresql']['db_superuser']
    end

    def legacy_migration_state_file
      @legacy_migration_state_file || @migration_state_file
    end

    def running_server
      return @running_server if @running_server

      if File.exists?(RUNNING_CONFIG_FILE)
        @running_server = JSON.parse(IO.read(RUNNING_CONFIG_FILE))
      else
        STDERR.puts <<EOF
***
ERROR: Cannot find #{RUNNING_CONFIG_FILE}
***
Try running `chef-server-ctl reconfigure` first.

EOF
        exit(1)
      end
    end

    def secrets
      return @secrets if @secrets

      if File.readable?(SECRETS_FILE)
        require 'veil'
        @secrets = Veil::CredentialCollection::ChefSecretsFile.from_file(SECRETS_FILE)
      else
        STDERR.puts <<EOF
***
ERROR: Cannot find or access #{SECRETS_FILE}
***
Try running `chef-server-ctl reconfigure` first.

EOF
        exit(1)
      end
    end

  end

  module Logger

    def log(message)
      puts "[private-chef-upgrade] - #{message}"
    end

  end
end
