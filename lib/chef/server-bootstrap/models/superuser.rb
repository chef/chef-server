######################################################################
# the superuser model
######################################################################

require 'chef/server-bootstrap/models/user'

module Chef::ServerBootstrap::Models
  class Superuser < User

    attr_reader :authz_id

    include Chef::ServerBootstrap::DatabaseHelper

    def create
      user_mapper = Opscode::Mappers::User.new(db, nil, Chef::ServerBootstrap.config.bifrost_superuser_id)
      db_user = Opscode::Models::User.new(to_hash)
      user_mapper.create(db_user)
      @authz_id = db_user.authz_id
    end
  end
end
