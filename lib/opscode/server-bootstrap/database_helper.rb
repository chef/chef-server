######################################################################
# helper module for configurable database params
######################################################################

require 'opscode/server-bootstrap/database_config'

module Opscode::ServerBootstrap
  module DatabaseHelper
    def db
      Opscode::ServerBootstrap.database_config.db
    end
  end
end

