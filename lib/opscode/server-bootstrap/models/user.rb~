######################################################################
# the user model
######################################################################

require 'opscode/mappers'

module Opscode::Test::Models
  class User

    attr_accessor :name
    attr_accessor :first_name
    attr_accessor :middle_name
    attr_accessor :last_name
    attr_accessor :display_name
    attr_accessor :email
    attr_accessor :password
    attr_accessor :certificate

    def create
      # TODO: use the api to create a user
    end

    private

    def to_hash
      {
        :username => name,
        :first_name => first_name,
        :middle_name => middle_name,
        :last_name => last_name,
        :display_name => display_name,
        :email => email,
        :password => password,
        :certificate => certificate.to_s
      }
    end
  end
end
