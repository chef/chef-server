######################################################################
# re-usable things, like postgres connections
######################################################################

require 'sequel'
require 'couchrest'
require 'opscode/test/config'

module Opscode::Test

  def self.database_config
    @database_config ||= DatabaseConfig.new
  end

  class DatabaseConfig

    include Configurable

    attr_reader :db

    def initialize
      @db = Sequel.connect("#{config.db_driver}://#{config.db_user}:#{config.db_password}@#{config.db_host}/opscode_chef")
    end

  end
end
