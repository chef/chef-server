module Partybus

  def self.config
    @config ||= Config.new
  end

  def self.configure(&block)
    yield(config)
  end

  class Config

    attr_accessor :database_connection_string
    attr_accessor :database_migration_directory
    attr_accessor :database_service_name
    attr_accessor :partybus_migration_directory
    attr_accessor :migration_state_file
    attr_accessor :private_chef_role
    attr_accessor :bootstrap_server

  end

  module Logger

    def log(message)
      puts "[private-chef-upgrade] - #{message}"
    end

  end
end
