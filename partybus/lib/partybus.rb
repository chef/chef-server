module Partybus

  def self.config
    @config ||= Config.new
  end

  class Config

    attr_accessor :database_connection_string
    attr_accessor :database_migration_directory

  end

end
