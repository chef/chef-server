######################################################################
# the superuser model
######################################################################

require 'opscode/test/models/user'

module Opscode::Test::Models
  class Superuser < User

    attr_reader :authz_id

    include Opscode::Test::DatabaseHelper

    def create
      user_mapper = Opscode::Mappers::User.new(db, nil, Opscode::Test.config.bifrost_superuser_id)
      db_user = Opscode::Models::User.new(to_hash)
      user_mapper.create(db_user)
      @authz_id = db_user.authz_id
    end
  end
end
