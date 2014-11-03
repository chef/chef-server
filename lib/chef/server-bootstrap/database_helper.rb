######################################################################
# helper module for configurable database params
######################################################################

require 'chef/server-bootstrap/database_config'

module Chef::ServerBootstrap
  module DatabaseHelper
    def db
      Chef::ServerBootstrap.database_config.db
    end
  end
end

