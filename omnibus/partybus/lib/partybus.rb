require 'chef-utils/dist'

module Partybus

  def self.config
    @config ||= Config.new
  end

  def self.configure(&block)
    yield(config)
  end

  class Config

    SECRETS_FILE = "/etc/#{::ChefUtils::Dist::Org::LEGACY_CONF_DIR}/private-#{::ChefUtils::Dist::Infra::SHORT}-secrets.json"
    RUNNING_CONFIG_FILE = "/etc/#{::ChefUtils::Dist::Org::LEGACY_CONF_DIR}/#{::ChefUtils::Dist::Server::SERVER}-running.json"

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
      if File.exist?(RUNNING_CONFIG_FILE)
        @running_server = JSON.parse(IO.read(RUNNING_CONFIG_FILE))
        @postgres = @running_server['private_chef']['postgresql']
      else
        STDERR.puts <<EOF
***
ERROR: Cannot find #{RUNNING_CONFIG_FILE}
***
Try running `chef-server-ctl reconfigure` first.

EOF
        exit(1)
      end
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
      puts "[Infra Server Upgrade] - #{message}"
    end

  end
end
