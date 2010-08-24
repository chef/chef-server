require 'eventmachine'
require 'em-http-request'
require 'mq'
require 'amqp'
require 'json'
require 'mixlib/log'
require 'chef/solr/index'

module Opscode
  module Expander

  end
end

require 'opscode/expander/loggable'

Opscode::Expander::Loggable::LOGGER.level = :debug