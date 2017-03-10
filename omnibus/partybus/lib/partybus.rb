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

    attr_accessor :database_connection_string
    attr_accessor :database_unix_user
    attr_accessor :database_migration_directory
    attr_accessor :database_service_name
    attr_accessor :partybus_migration_directory
    attr_accessor :migration_state_file
    attr_accessor :private_chef_role
    attr_accessor :is_data_master
    attr_accessor :bootstrap_server
    attr_accessor :couchdb_data_dir

    attr_accessor :running_server
    attr_accessor :postgres
    attr_accessor :secrets

    def initialize
      if File.exists?(RUNNING_CONFIG_FILE)
        @running_server = JSON.parse(IO.read(RUNNING_CONFIG_FILE))
        @postgres = @running_server['private_chef']['postgresql']
      else
        log <<EOF
***
ERROR: Cannot find #{RUNNING_CONFIG_FILE}
***
Try running `chef-server-ctl reconfigure` before upgrading.

EOF
        exit(1)
      end
      if File.readable?(SECRETS_FILE)
        require 'veil'
        @secrets = Veil::CredentialCollection::ChefSecretsFile.from_file(SECRETS_FILE)
      else
        log <<EOF
***
ERROR: Cannot find or access #{SECRETS_FILE}
***
Try running `chef-server-ctl reconfigure` before upgrading.

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
