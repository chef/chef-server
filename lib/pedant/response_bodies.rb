require 'pedant/acl'

module Pedant
  module ResponseBodies
    extend Pedant::Concern

    included do
      include Pedant::ACL
      # Cookbook endpoint overrides


    end # included
  end # ResponseBodies
end # Pedant
